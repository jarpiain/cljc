;; Copyright (c) Juha Arpiainen. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns org.subluminal.class-file
  (:require (org.subluminal [binfmt :as bin])
            (clojure string set)
            (clojure.contrib [graph :as graph]))
  (:use (clojure.contrib monads)
        (org.subluminal util))
  (:import (java.io ByteArrayInputStream DataInputStream InputStream
                    ByteArrayOutputStream DataOutputStream)
           (java.lang.reflect Field Method Constructor)
           (java.nio ByteBuffer)))

(declare to-symbol class-name pool-string disasm doasm
         sizeof-desc)

;;;; Parser for descriptor and signature strings

(declare <segments>)

(def <segments>
  (domonad parser-m
    [seg1 (peek-until #{\/ \; \< \.})
     sep (match-char \/)
     more (match-* #'<segments>)]
    (cons seg1 (first more))))

;; Parse descriptors

(declare <field-descriptor>)

(def <primitive>
  (match-case
    {\B :byte, \C :char, \D :double,
     \F :float, \I :int, \J :long,
     \S :short, \V :void, \Z :boolean}))

(def <class>
  (domonad parser-m
    [ch (match-char \L)
     name (match-until \;)]
    (to-symbol name)))

(def <array>
  (domonad parser-m
    [ch (match-char \[)
     f #'<field-descriptor>]
    [:array f]))

(def <field-descriptor>
  (with-monad parser-m
    (m-plus <primitive> <array> <class>)))

(def <class-constant>
  (with-monad parser-m
    (m-plus
      (domonad parser-m
        [_ (peek-char #{\[})
         d <field-descriptor>]
        d)
      match-tail)))

(def <method-descriptor>
  (domonad parser-m
    [lp (match-char \()
     args (match-* <field-descriptor>)
     rp (match-char \))
     ret <field-descriptor>]
    [:method ret args]))

;; Parse signatures

(declare <field-type-signature>
         <class-type-signature>
         <type-signature>)

(def <iface-bound>
  (domonad parser-m
    [colon (match-char \:)
     val #'<field-type-signature>]
    val))

(def <type-param>
  (domonad parser-m
    [id (match-until \:)
     class-bound (optional #'<field-type-signature>)
     iface-bound (match-* <iface-bound>)]
    (vector (to-symbol id)
            class-bound
            iface-bound)))

(def <type-params>
  (domonad parser-m
    [lp (match-char \<)
     val (match-+ <type-param>)
     rp (match-char \>)]
    val))

(def <class-signature>
  (domonad parser-m
    [type-params (optional <type-params>)
     super <class-type-signature>
     ifaces (match-* #'<class-type-signature>)]
    (list 'class
          type-params
          (list 'extends super)
          (list 'implements ifaces))))

(def <type-variable-signature>
  (domonad parser-m
    [ch (match-char \T)
     id (match-until \;)]
    (list 'var (to-symbol id))))

(def <array-type-signature>
  (domonad parser-m
    [ch (match-char \[)
     sig #'<type-signature>]
    (list 'array sig)))

(def <type-argument>
  (with-monad parser-m
    (m-plus (match-case {\* '*})
      (domonad parser-m
        [indicator (optional (match-case {\+ '+ \- '-}))
         bound #'<field-type-signature>]
        (if indicator
          (vector indicator bound)
          bound)))))

(def <type-arguments>
  (domonad parser-m
    [lp (match-char \<)
     args (match-+ <type-argument>)
     rp (match-char \>)]
    args))

(def <simple-class-type-sig>
  (domonad parser-m
    [id (peek-until #{\< \; \.})
     args (optional <type-arguments>)]
    (if args (cons (to-symbol id) args) (to-symbol id))))

(def <suffix>
  (domonad parser-m
    [ch (match-char \.)
     val <simple-class-type-sig>]
    val))

(def <class-type-signature>
  (domonad parser-m
    [ch (match-char \L)
     pkg <segments>
     top <simple-class-type-sig>
     suff (match-* <suffix>)
     endch (match-char \;)]
    (cons
      (to-symbol (clojure.string/join "/" pkg))
      (cons top suff))))

(def <field-type-signature>
  (with-monad parser-m
    (m-plus <class-type-signature>
            <array-type-signature>
            <type-variable-signature>)))

(def <type-signature>
  (with-monad parser-m
    (m-plus <field-type-signature>
            <primitive>)))

(def <throws-signature>
  (with-monad parser-m
    (domonad parser-m
      [caret (match-char \^)
       sig (m-plus <class-type-signature> <type-variable-signature>)]
      sig)))

(def <method-type-signature>
  (with-monad parser-m
    (domonad parser-m
      [types (optional <type-params>)
       lp (match-char \()
       args (match-* <type-signature>)
       rp (match-char \))
       ret <type-signature>
       throws (match-* <throws-signature>)]
      (list 'method
            types
            ret
            args
            (cons 'throws throws)))))

(def <any-signature>
  (with-monad parser-m
    (m-plus <method-type-signature> <class-signature> <field-type-signature>)))

(defn full-parse
  "Parse input string with the parser. Input must be fully consumed."
  [parser input]
  (let [[res remain :as whole] (parser input)]
    (if (or (nil? whole)
            (not (empty? remain)))
      (throw (IllegalArgumentException. (str "Can't parse '" input \')))
      res)))

;;;; Class file format
;; Compare "The Java(TM) Virtual Machine Specification", chapter 4

(bin/defbinary ClassFile
  [:magic ::bin/uint32 {:constraint #(= % 0xCAFEBABE)
                        :aux 0xCAFEBABE}]
  [:minor-version ::bin/uint16]
  [:major-version ::bin/uint16 {:constraint #(<= % 50)}]
  [:constant-pool-count ::bin/uint16 {:aux (count (:pool symtab))}]
  [:constant-pool [::cp-info constant-pool-count] {:aux (:pool symtab)}]
  [:symtab ::null {:transient {:pool []}}] ; (split-pool constant-pool)
  [:flags ::bin/uint16
     {:bitmask {:public 0 :final 4 :super 5 :interface 9 :abstract 10
                :synthetic 12 :annotation 13 :enum 14}
      :constraint (sat? (and (--> :annotation :interface)
                             (--> :interface
                                  (and :abstract
                                       (nor :final :super :enum)))
                             (nand :final :abstract)))}]
  [:this-class ::bin/uint16 {:constraint #(< 0 % constant-pool-count)}]
  [:name ::null
    {:transient (to-symbol (class-name constant-pool this-class))}]

  ;; super-class is zero for java.lang.Object
  [:super-class ::bin/uint16 {:constraint #(< % constant-pool-count)}]
  [:extends ::null
    {:transient (when-not (zero? super-class)
                  (to-symbol (class-name constant-pool super-class)))}]

  [:interfaces-count ::bin/uint16 {:aux (count implements)}]
  [:interfaces ::bin/uint16 {:times interfaces-count
                             :constraint #(< 0 % constant-pool-count)}]
  [:implements ::null
    {:transient (vec (map #(to-symbol (class-name constant-pool %))
                          interfaces))}]
  [:fields-count ::bin/uint16 {:aux (count fields)}]
  [:fields [::field-info constant-pool] {:times fields-count}]
  [:methods-count ::bin/uint16 {:aux (count methods)}]
  [:methods [::method-info constant-pool] {:times methods-count}]
  [:attributes-count ::bin/uint16 {:aux (count attributes)}]
  [:attributes [::attribute-info constant-pool] {:times attributes-count}])

(bin/defbinary [field-info pool]
  [:flags ::bin/uint16
    {:bitmask {:public 0 :private 1 :protected 2
               :static 3 :final 4 :volatile 6 :transient 7
               :synthctic 12 :enum 14}}]
  [:name-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
  [:name ::null {:transient (symbol (pool-string pool name-index))}]
  [:descriptor-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
  [:descriptor ::null {:transient (->> descriptor-index (pool-string pool)
                                       (full-parse <field-descriptor>))}]
  [:attributes-count ::bin/uint16 {:aux (count attributes)}]
  [:attributes [::attribute-info pool] {:times attributes-count}])

(bin/defbinary [method-info pool]
  [:flags ::bin/uint16
    {:bitmask {:public 0 :private 1 :protected 2
               :static 3 :final 4 :synchronized 5
               :bridge 6 :varargs 7 :native 8
               :abstract 10 :strict 11 :synthetic 12}}]
  [:name-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
  [:name ::null {:transient (symbol (pool-string pool name-index))}]
  [:descriptor-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
  [:descriptor ::null {:transient (->> descriptor-index (pool-string pool)
                                       (full-parse <method-descriptor>))}]
  [:attributes-count ::bin/uint16 {:aux (count attributes)}]
  [:attributes [::attribute-info pool] {:times attributes-count}])

(declare pool-length attribute-length)

(defn member-length
  "Calculates the length of the binary representation
  of a field or method in bytes"
  [memb]
  (+ 8 
     (* 6 (count (:attributes memb))) ; attr headers
     (reduce + (map (fn [at] (attribute-length at))
                      (:attributes memb)))))

(defn class-length
  "Calculates the length of the generated .class file in bytes"
  [cls]
  (+ 24
     (* 2 (count (:implements cls)))
     (* 6 (count (:attributes cls)))
     (pool-length (:pool (:symtab cls)))
     (reduce + (map member-length (:fields cls)))
     (reduce + (map member-length (:methods cls)))
     (reduce + (map (fn [at] (attribute-length at))
                    (:attributes cls)))))

;;;; Constant pool

(defn class-name
  "Look up the name corresponding to a Class_info constant"
  [pool ix]
  (let [name-ix (:name-index (pool ix))]
    (when name-ix
      (:val (pool name-ix)))))

(defn pool-string
  "Look up an Utf8_info constant"
  [pool ix]
  (:val (pool ix)))

(defn to-symbol
  "Returns a symbol naming a class, method or field"
  [^String txt]
  (symbol (.replace txt \/ \.)))

;; long and double constants use 2 pool indices
(bin/defprimitive cp-info [buf obj size]
  (loop [src (cons [nil] (repeatedly (fn [] (bin/read-binary ::cp-info1 buf))))
         acc []]
    (if (= (count acc) size)
      acc
      (let [{tag :tag :as obj} (first src)
            seg (case tag
                  (:long :double) [obj nil]
                  [obj])]
        (recur (rest src) (into acc seg)))))

  (doseq [inf obj]
    (when inf
      (bin/write-binary ::cp-info1 buf inf))))

(bin/defprimitive utf8 [buf ^String obj]
  (let [size (bin/read-binary ::bin/uint16 buf)]
    (.position buf (- (.position buf) 2))
    (let [bytes (bin/read-binary ::bin/byte-array buf (+ size 2))
          dis (DataInputStream. (ByteArrayInputStream. bytes))]
      (.readUTF dis)))

  ;; Size is accurate only for the ASCII subset
  (let [baos (ByteArrayOutputStream. (+ (count obj) 2))
        dos (DataOutputStream. baos)]
    (.writeUTF dos obj)
    (.put buf (.toByteArray baos))))

(bin/defbinary cp-info1
  [:tag ::bin/uint8 {:xenum {:class 7 :field 9 :method 10
                             :interface-method 11 :string 8
                             :integer 3 :float 4 :long 5 :double 6
                             :name-and-type 12 :utf8 1}}]
  (case tag
    :class [:name-index ::bin/uint16]
    :field (do [:class-index ::bin/uint16]
               [:name-type-index ::bin/uint16])
    :method (do [:class-index ::bin/uint16]
                [:name-type-index ::bin/uint16])
    :interface-method
    (do [:class-index ::bin/uint16]
        [:name-type-index ::bin/uint16])
    :string [:string-index ::bin/uint16]
    :integer [:val ::bin/int32]
    :float [:val ::bin/single-float]
    :long [:val ::bin/int64]
    :double [:val ::bin/double-float]
    :name-and-type
    (do [:name-index ::bin/uint16]
        [:descriptor-index ::bin/uint16])
    :utf8 [:val ::utf8]))

(defn utf8-len [txt]
  (let [baos (ByteArrayOutputStream. (+ 2 (count txt)))
        dos (DataOutputStream. baos)]
    (.writeUTF dos txt)
    (.size baos)))

(defn pool-entry-length [cp-inf]
  (if-not cp-inf 0
    (case (:tag cp-inf)
      :class 3
      :field 5
      :method 5
      :interface-method 5
      :string 3
      :integer 5
      :float 5
      :long 9
      :double 9
      :name-and-type 5
      :utf8 (inc (utf8-len (:val cp-inf))))))

(defn pool-length [pool]
  (reduce + (map pool-entry-length pool)))

(defn lookup-field
  "Lookup symbolic reprepentation of field name and type
   from a Fieldref_info constant"
  [pool idx]
  (let [fld (pool idx)
        nti (:name-type-index fld)
        cli (:class-index fld)
        cls (to-symbol (class-name pool cli))
        nt (pool nti)
        name (to-symbol (:val (pool (:name-index nt))))
        descr (full-parse <field-descriptor>
                          (:val (pool (:descriptor-index nt))))]
    [cls name descr]))

(defn lookup-method
  "Lookup symbolic representation of method name and type
   from a Methodref_info or InterfaceMethodref_info constant"
  [pool idx]
  (let [meth (pool idx)
        nti (:name-type-index meth)
        cli (:class-index meth)
        cls (to-symbol (class-name pool cli))
        nt (pool nti)
        name (to-symbol (:val (pool (:name-index nt))))
        descr (full-parse <method-descriptor>
                          (:val (pool (:descriptor-index nt))))]
    [cls name descr]))

;;;; Attributes

;; Used to calculate the length field when writing
;; Add 6 to get the full length
(defmulti attribute-length :name)
(defmethod attribute-length :default
  [inf]
  (println "Unknown attribute!")
  0)

(defmethod attribute-length "ConstantValue"
  [inf]
  2)

(defmethod attribute-length "Code"
  [inf]
  (+ 12
     (:code-length inf)
     (* 8 (count (:exception-table inf)))
     (* 6 (count (:attributes inf))) ; headers
     (reduce + (map #(attribute-length (:name %) %)
                    (:attributes inf)))))

(defmethod attribute-length "Exceptions"
  [inf]
  (+ 2 (* 2 (count (:index-table inf)))))

(defmethod attribute-length "SourceFile"
  [inf]
  2)

(defmethod attribute-length "Deprecated"
  [inf]
  0)

(defmethod attribute-length "Synthetic"
  [inf]
  0)

(defmethod attribute-length "LineNumberTable"
  [inf]
  (+ 2 (* 4 (count (:table inf)))))

(bin/defbinary [attribute-info pool]
  [:name-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
  [:name ::null {:transient (pool-string pool name-index)}]
  [:length ::bin/uint32 {:aux (attribute-length attribute-info)}]
  [:len2 ::null {:transient length}]
  (cond
    (= name "SourceFile")
    (do [:file-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
        [:file-name ::null {:transient (pool-string pool file-index)}])

    (= name "ConstantValue")
    (do [:value-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
        [:value ::null {:transient (pool value-index) :constraint identity}])

    (= name "Code")
    (do [:max-stack ::bin/uint16]
        [:max-locals ::bin/uint16]
        [:code-length ::bin/uint32]
        [:code [::bin/byte-array code-length]
           {:aux (doasm asm (:labels attribute-info) code-length)}]
        [:asm ::null
           {:transient (try (disasm code pool)
                         (catch Exception e (.printStackTrace e)))}]
        [:extab-length ::bin/uint16 {:aux (count exception-table)}]
        [:exception-table [::exception-table pool] {:times extab-length}]
        [:attributes-count ::bin/uint16 {:aux (count attributes)}]
        [:attributes [::attribute-info pool] {:times attributes-count}])

    (= name "Exceptions")
    (do [:num-exceptions ::bin/uint16 {:aux (count index-table)}]
        [:index-table ::bin/uint16 {:times num-exceptions}]
        [:exceptions ::null
          {:transient (vec (map #(to-symbol (class-name pool %))
                                index-table))}])

    (= name "LineNumberTable")
    (do [:table-length ::bin/uint16 {:aux (count table)}]
        [:table ::line-number-info {:times table-length}])

    (= name "LocalVariableTable")
    (do [:table-length ::bin/uint16 {:aux (count table)}]
        [:table [::local-var-info pool] {:times table-length}])

    (= name "LocalVariableTypeTable")
    (do [:table-length ::bin/uint16 {:aux (count table)}]
        [:table [::local-var-type-info pool]
                {:times table-length}])

    (= name "InnerClasses")
    (do [:num-classes ::bin/uint16 {:aux (count classes)}]
        [:classes [::inner-class pool] {:times num-classes}])

    (= name "EnclosingMethod")
    (do [:class-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
        [:class ::null {:transient (class-name pool class-index)}]
        [:method-index ::bin/uint16 {:constraint #(< 0 % (count pool))}])

    (= name "Signature")
    (do [:signature-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
        [:sig ::null {:transient (full-parse <any-signature>
                                   (pool-string pool signature-index))}])

    #_(= name "SourceDebugExtension")
    #_[:debug-extension ::utf8]

    (= name "Deprecated")
    (do)

    (= name "Synthetic")
    (do)

    (= name "RuntimeVisibleAnnotations")
    (do [:num-annotations ::bin/uint16 {:aux (count annotations)}]
        [:annotations [::annotation pool] {:times num-annotations}])
    
    (= name "RuntimeInvisibleAnnotations")
    (do [:num-annotations ::bin/uint16 {:aux (count annotations)}]
        [:annotations [::annotation pool] {:times num-annotations}])

    (= name "RuntimeVisibleParameterAnnotations")
    (do [:num-parameters ::bin/uint16 {:aux (count parameters)}]
        [:parameters [::parameter-annotations pool] {:times num-parameters}])

    (= name "RuntimeInvisibleParameterAnnotations")
    (do [:num-parameters ::bin/uint16 {:aux (count parameters)}]
        [:parameters [::parameter-annotations pool] {:times num-parameters}])

    (= name "AnnotationDefault")
    [:default-value [::element-value pool]]

    (= name "StackMapTable")
    (do [:num-entries [::bin/uint16] {:aux (count entries)}]
        [:entries [::stack-map-entry pool] {:times num-entries}])

    :else
    [:info [::bin/byte-array length]]))

;; StackMapTable attribute

(defn frame-tag [frame-type]
  (cond (<= 0 frame-type 63) :same
        (<= 64 frame-type 127) :same-locals-1-stack-item
        (= frame-type 247) :same-locals-1-stack-item-extended
        (<= 248 frame-type 250) :chop
        (= frame-type 251) :same-extended
        (<= 252 frame-type 254) :append
        (= frame-type 255) :full))

(bin/defbinary [stack-map-entry pool]
  [:frame-type ::bin/uint8]
  [:tag ::null {:transient (frame-tag frame-type)}]
  (cond
    (<= 0 frame-type 63) ; same-frame
    (do [:offset-delta ::null {:transient frame-type}])
    (<= 64 frame-type 127) ; same-locals-1-stack-item-frame
    (do
      [:stack [::verification-type pool] {:times 1}]
      [:offset-delta ::null {:transient (- frame-type 64)}])

    (= frame-type 247) ; same-locals-1-stack-item-frame-extended
    (do [:offset-delta ::bin/uint16]
        [:stack [::verification-type pool] {:times 1}])

    (<= 248 frame-type 250) ; chop-frame
    (do [:locals-chopped ::null {:transient (- 251 frame-type)}]
        [:offset-delta ::bin/uint16])

    (= frame-type 251) ; same-frame-extended
    [:offset-delta ::bin/uint16]

    (<= 252 frame-type 254) ; append-frame
    (do [:offset-delta ::bin/uint16]
        [:locals [::verification-type pool] {:times (- frame-type 251)}])

    (= frame-type 255) ; full-frame
    (do [:offset-delta ::bin/uint16]
        [:num-locals ::bin/uint16 {:aux (count locals)}]
        [:locals [::verification-type pool] {:times num-locals}]
        [:num-stack ::bin/uint16 {:aux (count stack)}]
        [:stack [::verification-type pool] {:times num-stack}])))

(bin/defbinary [verification-type pool]
  [:tag ::bin/uint8 {:enum {:top 0 :int 1 :float 2 :long 4
                            :double 3 :null 5 :uninitialized-this 6
                            :object 7 :uninitialized 8}}]
  (cond
    (= tag :object)
    (do [:class-index ::bin/uint16 {:aux 0}]
        [:class ::null {:transient (to-symbol (class-name pool class-index))}])
    (= tag :uninitialized)
    [:offset ::bin/uint16]
    :default
    (do)))

;; Annotations attribute

(bin/defbinary [parameter-annotations pool]
  [:num-annotations ::bin/uint16 {:aux (count parameter-annotations)}]
  [internal [::annotation pool] {:times num-annotations}])

(bin/defbinary [element-value pool]
  [:itag ::bin/uint8 {:aux (int tag)}]
  [:tag ::null {:transient (char itag)}]
  (cond
    ;; base type
    (#{\B\C\D\F\I\J\S\Z\s} tag)
    (do [:value-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
        [:value ::null {:transient (pool value-index) :constraint identity}])

    (= tag \e)
    (do [:enum-type-index ::bin/uint16 {:constraint #(< 0 % (count pool))
                                        :aux 0}]
        [:enum-type ::null {:transient (full-parse <field-descriptor>
                                         (pool-string pool enum-type-index))}]
        [:enum-val-index ::bin/uint16 {:constraint #(< 0 % (count pool))}]
        [:enum-val ::null {:transient (to-symbol
                                        (pool-string pool enum-val-index))}])

    (= tag \c)
    (do [:class-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
        [:class ::null {:transient (full-parse <field-descriptor>
                                     (pool-string pool class-index))}])

    (= tag \@)
    [:annotation-value ::annotation]

    (= tag \[)
    (do [:length ::bin/uint16 {:aux (count values)}]
        [:values ::element-value {:times length}])))

(bin/defbinary [attribute-property pool]
  [:name-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
  [:name ::null {:transient (to-symbol (pool-string pool name-index))}]
  [:value [::element-value pool]])

(bin/defbinary [annotation pool]
  [:type-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
  [:type ::null {:transient (full-parse <field-descriptor>
                                        (:val (pool type-index)))}]
  [:num-value-pairs ::bin/uint16 {:aux (count value-pairs)}]
  [:value-pairs [::attribute-property pool] {:times num-value-pairs}])

;; InnerClasses attribute

(bin/defbinary [inner-class pool]
  [:inner-class-info-index ::bin/uint16 {:constraint #(< 0 % (count pool))
                                         :aux 0}]
  [:inner ::null {:transient (to-symbol
                               (class-name pool inner-class-info-index))}]
  [:outer-class-info-index ::bin/uint16 {:constraint #(< % (count pool))}]
  (if (not (zero? outer-class-info-index))
    [:outer ::null {:transient (to-symbol
                                 (class-name pool outer-class-info-index))}])
  [:inner-name-index ::bin/uint16 {:constraint #(< % (count pool))}]
  (if (not (zero? inner-name-index))
    [:inner-name ::null {:transient (pool-string pool inner-name-index)}])
  [:inner-class-access-flags ::bin/uint16
     {:bitmask {:public 0 :private 1 :protected 2
                :static 3 :final 4 :interface 9 :abstract 10
                :synthetic 12 :annotation 13 :enum 14}}])

;; LocalVarTable, LineNumberTable, Exceptions

(bin/defbinary [local-var-info pool]
  [:start-pc ::bin/uint16]
  [:length ::bin/uint16]
  [:name-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
  [:name ::null {:transient (to-symbol (pool-string pool name-index))}]
  [:descriptor-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
  [:descriptor ::null
     {:transient (full-parse <field-descriptor>
                             (pool-string pool descriptor-index))}]
  [:index ::bin/uint16])

(bin/defbinary [local-var-type-info pool]
  [:start-pc ::bin/uint16]
  [:length ::bin/uint16]
  [:name-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
  [:name ::null {:transient (pool-string pool name-index)}]
  [:signature-index ::bin/uint16 {:constraint #(< 0 % (count pool)) :aux 0}]
  [:sig ::null {:transient (full-parse <field-type-signature>
                             (pool-string pool signature-index))}]
  [:index ::bin/uint16])

(bin/defbinary line-number-info
  [:start-pc ::bin/uint16]
  [:line-number ::bin/uint16])

(bin/defbinary [exception-table pool]
  [:start-pc ::bin/uint16]
  [:end-pc ::bin/uint16]
  [:handler-pc ::bin/uint16]
  [:catch-type ::bin/uint16 {:constraint #(< % (count pool))}]
  [:catch ::null {:transient (if (zero? catch-type)
                               '* ; finally
                               (to-symbol (class-name pool catch-type)))}])

;;;; Bytecode disassembler

(bin/alias-tag! ::bin/uint8  ::local-var)
(bin/alias-tag! ::bin/uint16 ::pool-class)
(bin/alias-tag! ::bin/uint16 ::pool-field)
(bin/alias-tag! ::bin/uint16 ::pool-method)
(bin/alias-tag! ::bin/uint16 ::pool-iface-method)
(bin/alias-tag! ::bin/uint16 ::pool-name-type)
(bin/alias-tag! ::bin/uint8  ::pool-constant8)
(bin/alias-tag! ::bin/uint16 ::pool-constant16)

;; The zero byte of :invokeinterface
(bin/defprimitive zero [buf obj]
  (.get buf)
  (.put buf (byte 0)))

;; Padding for :tableswitch and :lookupswitch
(bin/defprimitive pad4 [buf obj]
  (.position buf
    (bin/pad4 (.position buf)))
  (.position buf
    (bin/pad4 (.position buf))))

;; Type codes for :newarray
(bin/defprimitive primitive-tag [buf obj]
  ({4 :boolean, 5 :char, 6 :float, 7 :double,
    8 :byte, 9 :short, 10 :int, 11 :long}
    (.get buf))
  (.put buf
    (byte ({:boolean 4, :char 5, :float 6, :double 7,
            :byte 8, :short 9, :int 10, :long 11} obj))))

(def
  ^{:doc "Opcodes and argument types for JVM instructions"}
  +opcodes+
  {:aaload      [0x32]
   :aastore     [0x53]
   :aconst-null [0x01]
   :aload       [0x19 ::local-var]
   :aload-0     [0x2a]
   :aload-1     [0x2b]
   :aload-2     [0x2c]
   :aload-3     [0x2d]
   :anewarray   [0xbd ::pool-class]
   :areturn     [0xb0]
   :arraylength [0xbe]
   :astore      [0x3a ::local-var]
   :astore-0    [0x4b]
   :astore-1    [0x4c]
   :astore-2    [0x4d]
   :astore-3    [0x4e]
   :athrow      [0xbf]
   :baload      [0x33]
   :bastore     [0x54]
   :bipush      [0x10 ::bin/int8]
   :caload      [0x34]
   :castore     [0x55]
   :checkcast   [0xc0 ::pool-class]
   :d2f         [0x90]
   :d2i         [0x8e]
   :d2l         [0x8f]
   :dadd        [0x63]
   :daload      [0x31]
   :dastore     [0x52]
   :dcmpg       [0x98]
   :dcmpl       [0x97]
   :dconst-0    [0x0e]
   :dconst-1    [0x0f]
   :ddiv        [0x6f]
   :dload       [0x18 ::local-var]
   :dload-0     [0x26]
   :dload-1     [0x27]
   :dload-2     [0x28]
   :dload-3     [0x29]
   :dmul        [0x6b]
   :dneg        [0x77]
   :drem        [0x73]
   :dreturn     [0xaf]
   :dstore      [0x39 ::local-var]
   :dstore-0    [0x47]
   :dstore-1    [0x48]
   :dstore-2    [0x49]
   :dstore-3    [0x4a]
   :dsub        [0x67]
   :dup         [0x59]
   :dup-x1      [0x5a]
   :dup-x2      [0x5b]
   :dup2        [0x5c]
   :dup2-x1     [0x5d]
   :dup2-x2     [0x5e]
   :f2d         [0x8d]
   :f2i         [0x8b]
   :f2l         [0x8c]
   :fadd        [0x62]
   :faload      [0x30]
   :fastore     [0x51]
   :fcmpg       [0x96]
   :fcmpl       [0x95]
   :fconst-0    [0x0b]
   :fconst-1    [0x0c]
   :fconst-2    [0x0d]
   :fdiv        [0x6e]
   :fload       [0x17 ::local-var]
   :fload-0     [0x22]
   :fload-1     [0x23]
   :fload-2     [0x24]
   :fload-3     [0x25]
   :fmul        [0x6a]
   :fneg        [0x76]
   :frem        [0x72]
   :freturn     [0xae]
   :fstore      [0x38 ::local-var]
   :fstore-0    [0x43]
   :fstore-1    [0x44]
   :fstore-2    [0x45]
   :fstore-3    [0x46]
   :fsub        [0x66]
   :getfield    [0xb4 ::pool-field]
   :getstatic   [0xb2 ::pool-field]
   :goto        [0xa7 ::bin/int16]
   :goto-w      [0xc8 ::bin/int32]
   :i2b         [0x91]
   :i2c         [0x92]
   :i2d         [0x87]
   :i2f         [0x86]
   :i2l         [0x85]
   :i2s         [0x93]
   :iadd        [0x60]
   :iaload      [0x2e]
   :iand        [0x7e]
   :iastore     [0x4f]
   :iconst-m1   [0x02]
   :iconst-0    [0x03]
   :iconst-1    [0x04]
   :iconst-2    [0x05]
   :iconst-3    [0x06]
   :iconst-4    [0x07]
   :iconst-5    [0x08]
   :idiv        [0x6c]
   :if-acmpeq   [0xa5 ::bin/int16]
   :if-acmpne   [0xa6 ::bin/int16]
   :if-icmpeq   [0x9f ::bin/int16]
   :if-icmpne   [0xa0 ::bin/int16]
   :if-icmplt   [0xa1 ::bin/int16]
   :if-icmpge   [0xa2 ::bin/int16]
   :if-icmpgt   [0xa3 ::bin/int16]
   :if-icmple   [0xa4 ::bin/int16]
   :ifeq        [0x99 ::bin/int16]
   :ifne        [0x9a ::bin/int16]
   :iflt        [0x9b ::bin/int16]
   :ifge        [0x9c ::bin/int16]
   :ifgt        [0x9d ::bin/int16]
   :ifle        [0x9e ::bin/int16]
   :ifnonnull   [0xc7 ::bin/int16]
   :ifnull      [0xc6 ::bin/int16]
   :iinc        [0x84 ::local-var ::bin/int8]
   :iload       [0x15 ::local-var]
   :iload-0     [0x1a]
   :iload-1     [0x1b]
   :iload-2     [0x1c]
   :iload-3     [0x1d]
   :imul        [0x68]
   :ineg        [0x74]
   :instanceof  [0xc1 ::pool-class]
   :invokedynamic   [0xba ::pool-name-type]
   :invokeinterface [0xb9 ::pool-iface-method ::bin/uint8 ::zero]
   :invokespecial   [0xb7 ::pool-method]
   :invokestatic    [0xb8 ::pool-method]
   :invokevirtual   [0xb6 ::pool-method]
   :ior         [0x80]
   :irem        [0x70]
   :ireturn     [0xac]
   :ishl        [0x78]
   :ishr        [0x7a]
   :istore      [0x36 ::local-var]
   :istore-0    [0x3b]
   :istore-1    [0x3c]
   :istore-2    [0x3d]
   :istore-3    [0x3e]
   :isub        [0x64]
   :iushr       [0x7c]
   :ixor        [0x82]
   :jsr         [0xa8 ::bin/int16]
   :jsr-w       [0xc9 ::bin/int32]
   :l2d         [0x8a]
   :l2f         [0x89]
   :l2i         [0x88]
   :ladd        [0x61]
   :laload      [0x2f]
   :land        [0x7f]
   :lastore     [0x50]
   :lcmp        [0x94]
   :lconst-0    [0x09]
   :lconst-1    [0x0a]
   :ldc         [0x12 ::pool-constant8]
   :ldc-w       [0x13 ::pool-constant16]
   :ldc2-w      [0x14 ::pool-constant16]
   :ldiv        [0x6d]
   :lload       [0x16 ::local-var]
   :lload-0     [0x1e]
   :lload-1     [0x1f]
   :lload-2     [0x20]
   :lload-3     [0x21]
   :lmul        [0x69]
   :lneg        [0x75]
   :lookupswitch [0xab ::pad4 ::bin/int32 ::bin/int32]
   :lor         [0x81]
   :lrem        [0x71]
   :lreturn     [0xad]
   :lshl        [0x79]
   :lshr        [0x7b]
   :lstore      [0x37 ::local-var]
   :lstore-0    [0x3f]
   :lstore-1    [0x40]
   :lstore-2    [0x41]
   :lstore-3    [0x42]
   :lsub        [0x65]
   :lushr       [0x7d]
   :lxor        [0x83]
   :monitorenter [0xc2]
   :monitorexit  [0xc3]
   :multianewarray [0xc5 ::pool-class ::bin/uint8]
   :new         [0xbb ::pool-class]
   :newarray    [0xbc ::primitive-tag]
   :nop         [0x00]
   :pop         [0x57]
   :pop2        [0x58]
   :putfield    [0xb5 ::pool-field]
   :putstatic   [0xb3 ::pool-field]
   :ret         [0xa9 ::local-var]
   :return      [0xb1]
   :saload      [0x35]
   :sastore     [0x56]
   :sipush      [0x11 ::bin/int16]
   :swap        [0x5f]
   :tableswitch [0xaa ::pad4 ::bin/int32 ::bin/int32 ::bin/int32]
   :wide        [0xc4]})

(def 
  ^{:doc "Argument types for instructions modified by the :wide instruction"}
  +wide-ops+
  {:iload [::bin/uint16]
   :fload [::bin/uint16]
   :aload [::bin/uint16]
   :lload [::bin/uint16]
   :dload [::bin/uint16]
   :istore [::bin/uint16]
   :fstore [::bin/uint16]
   :astore [::bin/uint16]
   :lstore [::bin/uint16]
   :dstore [::bin/uint16]
   :ret  [::bin/uint16]
   :iinc [::bin/uint16 ::bin/int16]})

(def +lookup-ops+
  (into {}
    (map (fn [[sym [op & args]]]
           [op (vec (cons sym args))])
         +opcodes+)))

(def +lookup-wide+
  (into {}
    (map (fn [[sym [& args]]]
           (let [[op] (+opcodes+ sym)]
             [op (vec (cons sym args))]))
         +wide-ops+)))

(defn- lookup-pool
  "Lookup symbolic representation of an instruction argument
   from the constant pool"
  [pool typ raw]
  (case typ
    ::pool-class (to-symbol (class-name pool raw))
    ::pool-method (lookup-method pool raw)
    ::pool-iface-method (lookup-method pool raw)
    ::pool-field (lookup-field pool raw)
    (::pool-constant8 ::pool-constant16)
    (let [const (pool raw)]
      (case (:tag const)
        :string (pool-string pool (:string-index const))
        :class (to-symbol (class-name pool raw))
        const))
    raw))

(defn- dis1
  "Disassemble one bytecode instruction"
  [^ByteBuffer buf pool]
  (let [pc (.position buf)
        [op & atyps] (+lookup-ops+ (bin/read-binary ::bin/uint8 buf))
        raw-args (doall (map #(lookup-pool pool %
                                (bin/read-binary % buf))
                             atyps))]
    (case op
      :invokeinterface
      (let [[mspec _ _] raw-args]
        (vector pc op mspec))
      
      :tableswitch
      (let [[_ default low high] raw-args
            offsets (repeatedly (inc (- high low))
                      (fn [] (bin/read-binary ::bin/int32 buf)))]
        (apply vector pc op default low high offsets))

      :lookupswitch
      (let [[_ default npairs] raw-args
            pairs (repeatedly npairs
                    (fn [] [(bin/read-binary ::bin/int32 buf)
                            (bin/read-binary ::bin/int32 buf)]))]
        (apply vector pc op default npairs pairs))

      :wide
      (let [[wop & watyps]
            (+lookup-wide+ (bin/read-binary ::bin/uint8 buf))
            wargs (doall (map #(lookup-pool %
                                 (bin/read-binary % buf))
                              watyps))]
        (apply vector pc op wop wargs))

      ; default
      (apply vector pc op raw-args))))

(defn disasm
  "Disassemble the bytecode array of a method"
  [^"[B" bytecode pool]
  (let [^ByteBuffer buf (bin/buffer-wrap bytecode)]
    (loop [res []]
      (if (zero? (.remaining buf))
        res
        (recur (conj res (dis1 buf pool)))))))

;; Helper for parsing classes from jars and the filesystem

(defn bin-slurp
  "Read an InputStream into a byte array"
  [^InputStream input]
  (let [out (ByteArrayOutputStream.)
        buf (byte-array 4096)]
    (loop []
      (let [siz (.read input buf)]
        (if (< siz 0)
          (bin/buffer-wrap (.toByteArray out))
          (do
            (.write out buf 0 siz)
            (recur)))))))

(defn parse-class
  "Parses the .class file representation of a class object
   into clojure data structures. May not work for dynamically
   loaded classes since the .class file must exist on the
   classpath."
  [^Class cls]
  (let [name (str "/" (.replace (.getName cls) \. \/) ".class")]
    (with-open [input (.getResourceAsStream cls name)]
      (bin/read-binary ::ClassFile (bin-slurp input)))))

;;;; Assembler

;; A /type specifier/ is a Class object,
;; symbol naming a fully qualified class,
;; a vector [:array <component-type-specifier>],
;; or one of the symbols :int :long :void, etc.
;;
;; Note that :void can be only used as the return
;; type specifier in a method descriptor.
;;
;; The normalized form of a type specifier
;; doesn't contain any Class objects.
;;
;; Type specifiers can be used both as (field) type descriptors
;; (UTF8_info items formatted like "I" or "[[D"
;; or "Ljava/lang/Object;" in the constant pool) and as
;; class specifiers (Class_info items referring to a strings
;; formatted like "java/lang/Object" or "[I" or
;; "[Ljava/lang/Object;").

(defn type-specifier? [spec]
  (or (class? spec)
      (symbol? spec)
      (keyword? spec)
      (and (seq spec)
           (= (first spec) :array)
           (type-specifier? (second spec)))))

(defn normalized-clazz? [cls]
  (or (symbol? cls)  ; class or iface
      (string? cls)  ; class name
      (keyword? cls) ; primitive
      (and (sequential? cls)
           (= (first cls) :array)
           (normalized-clazz? (second cls)))))

(def +primitive-descriptors+
     {Byte/TYPE :byte Short/TYPE :short Integer/TYPE :int
      Long/TYPE :long Float/TYPE :float Double/TYPE :double
      Character/TYPE :char Void/TYPE :void Boolean/TYPE :boolean})

(defn normalize-type-specifier [desc]
  {:post [(normalized-clazz? %)]}
  (cond
    (class? desc)
    (let [^Class cls desc]
      (cond
        (.isPrimitive cls)
        (+primitive-descriptors+ cls)
        (.isArray cls)
        [:array (normalize-type-specifier (.getComponentType cls))]
        :else
        (symbol (.getName cls))))

    (and (sequential? desc) (= (first desc) :array))
    [:array (normalize-type-specifier (second desc))]

    (string? desc)
    (symbol desc)

    :else
    desc))

(defn type-descriptor-string [desc]
  {:pre (normalized-clazz? desc)}
  (cond
    (keyword? desc)
    (case desc
      :byte "B", :char "C",
      :double "D", :float "F",
      :int "I", :long "J"
      :short "S", :void "V",
      :boolean "Z")

    (symbol? desc)
    (str "L" (.replace (name desc) \. \/) ";")

    ;(class? desc)
    ;(class-type-descriptor (.getName desc))

    :else
    (let [[atag component] desc]
      (case atag :array (str "[" (type-descriptor-string component))))))

(defn class-descriptor-string [desc]
  {:pre (normalized-clazz? desc)}
  (if (symbol? desc)
    (.replace (name desc) \. \/)
    (type-descriptor-string desc)))

;; A /field specifier/ is either a java.lang.reflect.Field object
;; or a vector [owner name type] where owner is a class specifier,
;; name is a symbol and type is a type specifier. Field specifiers
;; are used as arguments of the (get/put)static and (get/put)field
;; instructions.

(defn normalized-fldspec? [fld]
  (let [[owner name desc] fld]
    (and owner name desc
         (normalized-clazz? owner)
         (symbol? name)
         (normalized-clazz? desc))))

(defn normalize-field-specifier [arg]
  (cond
    (instance? Field arg)
    (let [^Field fld arg
          fname (symbol (.getName fld))
          fclass (symbol (.getName (.getDeclaringClass fld)))
          fdesc (normalize-type-specifier (.getType fld))]
      [fclass fname fdesc])

    :else
    (let [[owner name desc] arg]
      [(normalize-type-specifier owner)
       name
       (normalize-type-specifier desc)])))

;; A /method descriptor/ is a vector
;; [:method <return-type> [<argument-type>*]]
;; where the return and argument-types are
;; type specifiers.
;;
;; Method descriptors are used in defining methods
;; and as parts of method specifiers.
;; The descriptor is encoded like
;; "([Ljava/lang/String;)V"

(defn normalized-methdesc? [meth]
  (and
    (sequential? meth)
    (let [[tag ret args] meth]
      (and (= tag :method)
           (normalized-clazz? ret)
           (every? normalized-clazz? args)))))

(defn normalize-method-descriptor [desc]
  {:post [(normalized-methdesc? %)]}
  (let [[_ ret args] desc]
    [:method (normalize-type-specifier ret)
             (vec (map normalize-type-specifier args))]))

(defn method-descriptor-string [desc]
  {:pre (normalized-methdesc? desc)}
  (let [[_ ret args] desc]
    (str "(" (apply str (map type-descriptor-string args)) ")"
         (type-descriptor-string ret))))

;; A /method specifier/ is a java.lang.reflect.Method,
;; a java.lang.reflect.Constructor, or a vector
;; [owner name desc] where owner is a class specifier,
;; name is a symbol and desc is a method descriptor.
;;
;; Method specifiers are used as arguments of the
;; invoke* instructions.

(defn normalized-methspec? [meth]
  (let [[owner name desc] meth]
    (and owner name desc
         (normalized-clazz? owner)
         (symbol? name)
         (normalized-methdesc? desc))))

(defn normalize-method-specifier [ms]
  (cond
    (instance? Method ms)
    (let [^Method meth ms
          mname (symbol (.getName meth))
          mclass (symbol (.getName (.getDeclaringClass meth)))
          mdesc (normalize-method-descriptor
                  [:method (.getReturnType meth)
                           (seq (.getParameterTypes meth))])]
      [mclass mname mdesc])

    (instance? Constructor ms)
    (let [^Constructor ctor ms
          mname '<init>
          mclass (symbol (.getName (.getDeclaringClass ctor)))
          mdesc (normalize-method-descriptor
                  [:method :void (seq (.getParameterTypes ctor))])]
      [mclass mname mdesc])

    :else
    (let [[owner name desc] ms]
      [(normalize-type-specifier owner)
       name
       (normalize-method-descriptor desc)])))

(def *assembling* {})

(def empty-symtab {:pool [nil]
                   :classes {}
                   :strings {}
                   :utf {}
                   :ints {}
                   :longs {}
                   :floats {}
                   :doubles {}
                   :fields {}
                   :methods {}
                   :imethods {}
                   :descriptors {}})

(def empty-class {:symtab empty-symtab
                  :major-version 49
                  :minor-version 0
                  :extends Object
                  :fields []
                  :methods []
                  :attributes []})

;; Monadic functions that find the index of a value from the constant pool,
;; adding it if necessary

(defn int-to-pool [val]
  {:pre [(instance? Integer val)]}
  (fn [tab]
    (if-let [idx (get (:ints tab) (int val))]
      [idx tab]
      (let [idx (count (:pool tab))
            ntab (assoc tab :pool (conj (:pool tab) {:tag :integer :val val})
                            :ints (assoc (:ints tab) val idx))]
        [idx ntab]))))

(defn long-to-pool [val]
  {:pre [(instance? Long val)]}
  (fn [tab]
    (if-let [idx (get (:longs tab) (long val))]
      [idx tab]
      (let [idx (count (:pool tab))
            ntab (assoc tab :pool (conj (:pool tab) {:tag :long :val val} nil)
                            :longs (assoc (:longs tab) val idx))]
        [idx ntab]))))

(defn float-to-pool [val]
  {:pre [(instance? Float val)]}
  (fn [tab]
    (if-let [idx (get (:floats tab) (float val))]
      [idx tab]
      (let [idx (count (:pool tab))
            ntab (assoc tab :pool (conj (:pool tab) {:tag :float :val val})
                            :floats (assoc (:floats tab) val idx))]
        [idx ntab]))))

(defn double-to-pool [val]
  {:pre [(instance? Double val)]}
  (fn [tab]
    (if-let [idx (get (:doubles tab) (double val))]
      [idx tab]
      (let [idx (count (:pool tab))
            ntab (assoc tab :pool (conj (:pool tab) {:tag :double :val val} nil)
                            :doubles (assoc (:doubles tab) val idx))]
        [idx ntab]))))

(defn utf-to-pool [s]
  {:pre [(instance? String s)]}
  (fn [tab]
    (if-let [idx (get (:utf tab) s)]
      [idx tab]
      (let [idx (count (:pool tab))
            ntab (assoc tab :pool (conj (:pool tab) {:tag :utf8 :val s})
                            :utf (assoc (:utf tab) s idx))]
        [idx ntab]))))

(defn string-to-pool [s]
  {:pre [(instance? String s)]}
  (fn [tab]
    (if-let [idx (get (:strings tab) s)]
      [idx tab]
      ((domonad state-m
         [stri (utf-to-pool s)
          pool (fetch-val :pool)
          _ (update-val :pool #(conj % {:tag :string :string-index stri}))
          _ (update-val :strings #(assoc % s (count pool)))]
         (count pool)) tab))))

;; Used both in Method_info and Field_info structures
(defn descriptor-string [desc]
  {:pre [(or (normalized-clazz? desc)
             (normalized-methdesc? desc))]}
  (if (and (sequential? desc) (= (first desc) :method))
    (method-descriptor-string desc)
    (type-descriptor-string desc)))

;; Name-and-type pool entry
(defn desc-to-pool [d]
  {:pre [(let [[na de] d]
           (and (symbol? na)
                (or (normalized-clazz? de)
                    (normalized-methdesc? de))))]}
  (fn [tab]
    (if-let [idx (get (:descriptors tab) d)]
      [idx tab]
      (let [[name desc] d]
        ((domonad state-m
           [namei (utf-to-pool (str name))
            desci (utf-to-pool (descriptor-string desc))
            pool (fetch-val :pool)
            _ (update-val :pool #(conj % {:tag :name-and-type
                                          :name-index namei
                                          :descriptor-index desci}))
            _ (update-val :descriptors #(assoc % d (count pool)))]
           (count pool)) tab)))))

(defn class-to-pool [cls]
  {:pre [(normalized-clazz? cls)]}
  (fn [tab]
    (if-let [idx (get (:classes tab) cls)]
      [idx tab]
      ((domonad state-m
         [stri (utf-to-pool (class-descriptor-string cls))
          pool (fetch-val :pool)
          _ (update-val :pool #(conj % {:tag :class :name-index stri}))
          _ (update-val :classes #(assoc % cls (count pool)))]
         (count pool)) tab))))

(defn field-to-pool [[fclass fname fdesc :as fld]]
  {:pre [(normalized-fldspec? fld)]}
  (fn [tab]
    (if-let [idx (get (:fields tab) fld)]
      [idx tab]
      ((domonad state-m
         [cli (class-to-pool fclass)
          desci (desc-to-pool [fname fdesc])
          pool (fetch-val :pool)
          _ (update-val :pool #(conj % {:tag :field
                                        :class-index cli
                                        :name-type-index desci}))
          _ (update-val :fields #(assoc % fld (count pool)))]
         (count pool)) tab))))

(defn method-to-pool [[mclass mname mdesc :as meth] key]
  {:pre [(normalized-methspec? meth)]}
  (fn [tab]
    (if-let [idx (get (key tab) meth)]
      [idx tab]
      ((domonad state-m
         [cli (class-to-pool mclass)
          desci (desc-to-pool [mname mdesc])
          pool (fetch-val :pool)
          _ (update-val :pool #(conj % {:tag (if (= key :methods)
                                               :method
                                               :interface-method)
                                        :class-index cli
                                        :name-type-index desci}))
          _ (update-val key #(assoc % meth (count pool)))]
         (count pool)) tab))))

;; Argument of :ldc instruction
(defmulti const-to-pool class)

;; Must be a class specifier
(defmethod const-to-pool :default [c]
  (class-to-pool (normalize-type-specifier c)))

(defmethod const-to-pool Integer [c] (int-to-pool c))
(defmethod const-to-pool Long [c] (long-to-pool c))
(defmethod const-to-pool Float [c] (float-to-pool c))
(defmethod const-to-pool Double [c] (double-to-pool c))
(defmethod const-to-pool String [c] (string-to-pool c))

(declare add-attribute)

(defn init-class [cls]
  (let [[res syms]
        (run-with (:symtab cls)
          [this-class (class-to-pool (normalize-type-specifier (:name cls)))
           super-class (if-let [ext (:extends cls)]
                         (class-to-pool (normalize-type-specifier ext))
                         (m-result 0))
           ifaces (m-seq (map (comp class-to-pool normalize-type-specifier)
                              (:implements cls)))
           [src atr]  (if-let [file (:source-file cls)]
                        (m-seq [(utf-to-pool file)
                                (utf-to-pool "SourceFile")])
                        (m-result nil))]
          (assoc cls
                 :attributes (if src
                               (conj (:attributes cls)
                                 {:name-index atr
                                  :name "SourceFile"
                                  :file-index src})
                               (:attributes cls))
                 :this-class this-class
                 :super-class super-class
                 :interfaces (vec ifaces)))]
    (assoc res :symtab syms)))

(defn add-field [cref {:keys [name descriptor flags constant] :as fld}]
  (dosync
    (let [[[namei desci] tab]
          ((domonad state-m
             [n (utf-to-pool (str name))
              d (utf-to-pool (type-descriptor-string
                               (normalize-type-specifier descriptor)))]
             [n d])
           (:symtab @cref))
          fref (ref (assoc fld :name-index namei
                               :descriptor-index desci))]
      (alter cref #(-> % (update-in [:fields] conj fref)
                         (assoc :symtab tab)))
      (if constant
        (let [[ci tab] ((const-to-pool constant) (:symtab @cref))]
          (alter cref assoc :symtab tab)
          (add-attribute cref fref {:name "ConstantValue"
                                    :value-index ci})))
      fref)))

(defn add-attribute [cref xref {:keys [name] :as attr}]
  (dosync
    (let [[namei tab] ((utf-to-pool name) (:symtab @cref))
          attr (assoc attr :name-index namei)]
      (alter xref update-in [:attributes] conj attr)
      (alter cref assoc :symtab tab)
      attr)))

(defn merge-locals [{:keys [vars size] :as ctx} bindings]
  (let [[vars size]
        (reduce (fn [[vs siz] [sym typ]]
                  (let [delta (sizeof-desc (normalize-type-specifier typ))]
                    [(assoc vs sym siz) (+ siz delta)]))
                [vars size]
                (partition 2 bindings))]
    (assoc ctx :size size :vars vars)))

(defn add-method [cref {:keys [name descriptor flags params throws] :as meth}]
  (dosync
    (let [[_ _ args :as desc] (normalize-method-descriptor descriptor)
          bindings (interleave (concat params (repeatedly gensym)) args)
          init-ctx (merge-locals {:vars {} :size 0}
                                 (if (flags :static)
                                   bindings
                                   (list* 'this (:name @cref) bindings)))
          [[namei desci] tab]
          ((domonad state-m
             [n (utf-to-pool (str name))
              d (utf-to-pool (method-descriptor-string desc))]
             [n d])
           (:symtab @cref))
          mref (ref (assoc meth :name-index namei
                                :attributes []
                                :descriptor-index desci))]
      (alter cref #(-> % (update-in [:methods] conj mref)
                         (assoc :symtab tab)))

      ;; Code attribute must come first
      (when-not (some #{:abstract :native} flags)
        (alter mref assoc :code  {:name "Code"
                                  :labels {}
                                  :pc [0 0]
                                  :max-stack 0
                                  :max-locals (:size init-ctx)
                                  :code-length 0
                                  :asm []
                                  :ctx init-ctx
                                  :exception-table []
                                  :line-numbers []
                                  :attributes []}))

      (when throws
        (let [[idx tab]
              ((with-monad state-m
                (m-map class-to-pool
                       (map normalize-type-specifier throws)))
               (:symtab @cref))]
          (alter cref assoc :symtab tab)
          (add-attribute cref mref {:name "Exceptions"
                                    :index-table (vec idx)})))

      mref)))

;;;; First pass of the assembler
;; Add instruction arguments to constant pool;
;; note stack usage of each instruction;
;; calculate an interval of possible offsets
;; for each label; make a few simple transformations
;; between short and long instruction forms

(defn advance [[min max]]
  (fn [code]
    (let [[l r] (:pc code)]
      [nil (assoc code :pc [(+ l min) (+ r max)])])))

(defn bound-interval?
  "Return true if low <= dest - src <= high
  whenever dest and src lie within specified intervals"
  [[s1 s2] [d1 d2] [low hi]]
  (and (<= low (- d1 s1) hi)
       (<= low (- d2 s1) hi)
       (<= low (- d1 s2) hi)
       (<= low (- d2 s2) hi)))

;; No arguments
(def +simple-instructions+
   #{:aaload :aastore :aconst-null :aload-0 :aload-1 :aload-2 :aload-3
     :areturn :arraylength :astore-0 :astore-1 :astore-2 :astore-3
     :athrow :baload :bastore :caload :castore :d2f :d2i :d2l :dadd
     :daload :dastore :dcmpg :dcmpl :dconst-0 :dconst-1 :ddiv
     :dload-0 :dload-1 :dload-2 :dload-3 :dmul :dneg :drem :dreturn
     :dstore-0 :dstore-1 :dstore-2 :dstore-3 :dsub :dup :dup-x1 :dup-x2
     :dup2 :dup2-x1 :dup2-x2 :f2d :f2i :f2l :fadd :faload :fastore
     :fcmpg :fcmpl :fconst-0 :fconst-1 :fconst-2 :fdiv :fload-0 :fload-1
     :fload-2 :fload-3 :fmul :fneg :frem :freturn :fstore-0 :fstore-1
     :fstore-2 :fstore-3 :fsub :i2b :i2c :i2d :i2f :i2l :i2s :iadd
     :iaload :iand :iastore :iconst-m1 :iconst-0 :iconst-1 :iconst-2
     :iconst-3 :iconst-4 :iconst-5 :idiv :iload-0 :iload-1 :iload-2
     :iload-3 :imul :ineg :ior :irem :ireturn :ishl :ishr :istore-0
     :istore-1 :istore-2 :istore-3 :isub :iushr :ixor :l2d :l2f :l2i
     :ladd :laload :land :lastore :lcmp :lconst-0 :lconst-1 :ldiv
     :lload-0 :lload-1 :lload-2 :lload-3 :lmul :lneg :lor :lrem
     :lreturn :lshl :lshr :lstore-0 :lstore-1 :lstore-2 :lstore-3 :lsub
     :lushr :lxor :monitorenter :monitorexit :nop :pop :pop2
     :return :saload :sastore :swap})

(defstruct instruction :op :args :stack)

(defn ins-size
  "Estimate size of one instruction.
  Called on first pass of the assembler."
  [pc {op :op, [target :as args] :args, :as ins} labels]
  (if (contains? +simple-instructions+ op)
    [1 1]
    (case op
      (:aload :astore :bipush :dload :dstore :fload :fstore :iload :istore
       :ldc :lload :lstore :newarray :ret)
      [2 2]
      (:anewarray :checkcast :getfield :getstatic :iinc :instanceof
       :invokedynamic :invokespecial :invokestatic :invokevirtual
       :ldc-w :ldc2-w :new :putfield :putstatic :sipush)
      [3 3]
      (:multinewarray)
      [4 4]
      (:invokeinterface :jsr-w)
      [5 5]
      (:goto :jsr :goto-w)
      (if (integer? target)
        (if (<= -0x8000 target 0x7FFF)
          [3 3]
          [5 5])

        (if-let [lbl (get labels target)]
          (if (bound-interval? pc lbl [-0x8000 0x7FFF])
            [3 3]
            [3 5])
          [3 5]))

      (:if-acmpeq :if-acmpne :if-icmpeq :if-icmpne :if-icmplt
       :if-icmpge :if-icmpgt :if-icmple :ifeq :ifne :iflt :ifge
       :ifgt :ifle :ifnonnull :ifnull)
      (if (integer? target)
        [3 3]
        (if-let [lbl (get labels target)]
          (if (bound-interval? pc lbl [-0x8000 0x7FFF])
            [3 3]
            [3 8])
          [3 8]))

      :lookupswitch
      (let [[_ default count] args 
            fix (+ 9 (* 8 count))]
        [fix (+ fix 3)])

      :tableswitch
      (let [[_ default low high] args
            fix (+ 13 (* 4 (inc (- high low))))]
        [fix (+ fix 3)])

      :wide
      (let [[op] args]
        (if (= op :iinc)
          [6 6]
          [4 4])))))

(defn add-label [lbl]
  (fn [code]
    (let [pc (:pc code)]
      [pc (assoc-in code [:labels lbl] pc)])))

(defn bump-locals [k]
  (fn [code]
    (let [prev (:max-locals code)]
      (if (> k prev)
        [nil (assoc code :max-locals k)]
        [nil code]))))

(defmulti normalize-arg (fn [typ arg] typ))

(defmethod normalize-arg ::pool-class
  [typ arg]
  (normalize-type-specifier arg))

(defmethod normalize-arg ::pool-method
  [typ arg]
  (normalize-method-specifier arg))

(defmethod normalize-arg ::pool-iface-method
  [typ arg]
  (normalize-method-specifier arg))

(defmethod normalize-arg ::pool-field
  [typ arg]
  (normalize-field-specifier arg))

(defmethod normalize-arg :default
  [typ arg]
  arg)

;; Does not convert labels to offsets (offset16, 32)
;; since the offset is not yet known
(defn lookup-instr-arg
  "Converts symbolic references and literal
  values to local variable and constant pool indices.
  Called on first pass of the assembler."
  [atyp asym locals]
;  (with-monad state-m)
    (case atyp
      ::local-var (with-monad state-m (m-result (locals asym)))
      ::pool-class (class-to-pool asym)
      ::pool-field (field-to-pool asym)
      ::pool-method (method-to-pool asym :methods)
      ::pool-iface-method (method-to-pool asym :imethods)
      ::pool-name-type (desc-to-pool asym)

      (::pool-constant8 ::pool-constant16)
      (const-to-pool asym)

      ::zero (with-monad state-m (m-result 0))
      ::pad4 (with-monad state-m (m-result nil))

      (::primitive-tag ::bin/int8 ::bin/uint8 ::bin/int32 ::bin/int16
       ::offset16 ::offset32)
      (with-monad state-m (m-result asym))))

;; [:astore 0] --> :astore-0, etc.
(defn load-store-op [op n]
  (keyword (str (name op) "-" n)))

(defn sizeof-desc [desc]
  (cond
    (= desc :long) 2
    (= desc :double) 2
    (= desc :void) 0
    :else 1))

(defn method-weight [[_ _ args]]
  (reduce + (map sizeof-desc args)))

(letfn [(--> [y xs] (zipmap xs (repeat y)))]
  (def +stack-delta+
    (merge
      (--> -1 [:aaload :baload :caload
               :iaload :faload :saload])
      (--> -3 [:aastore :bastore :castore
               :iastore :fastore :sastore])
      (--> +1 [:aload :aload-0 :aload-1
               :aload-2 :aload-3 :fload
               :fload-0 :fload-1 :fload-2
               :fload-3 :iload :iload-0
               :iload-1 :iload-2 :iload-3])
      (--> -1 [:astore :astore-0 :astore-1
               :astore-2 :astore-3 :fstore
               :fstore-0 :fstore-1 :fstore-2
               :fstore-3 :istore :istore-0
               :istore-1 :istore-2 :istore-3
               :areturn :ireturn :freturn
               :athrow :pop])
      (--> +0 [:return :ret])
      (--> +1 [:bipush :sipush])
      (--> +0 [:checkcast :instanceof])
      (--> +0 [:daload :laload])
      (--> -4 [:dastore :lastore])
      (--> -2 [:dadd :ddiv :dmul :drem :dsub])
      (--> +2 [:dload :dload-0 :dload-1
               :dload-2 :dload-3 :lload
               :lload-0 :lload-1 :lload-2
               :lload-3])
      (--> -2 [:dstore :dstore-0 :dstore-1 :dstore-2
               :dstore-3 :lstore :lstore-0 :lstore-1
               :lstore-2 :lstore-3 :dreturn :lreturn :pop2])
      (--> -1 [:d2f :d2i :l2f :l2i])
      (--> +1 [:f2d :i2d :f2l :i2l])
      (--> +0 [:l2d :d2l])
      (--> +0 [:i2b :i2c :i2f :i2s :f2i])
      (--> +0 [:dneg :fneg :ineg :lneg])
      (--> -3 [:dcmpg :dcmpl :lcmp])
      (--> -1 [:fcmpg :fcmpl])
      (--> +1 [:iconst-m1 :iconst-0 :iconst-1 :iconst-2
               :iconst-3 :iconst-4 :iconst-5 :aconst-null
               :fconst-0 :fconst-1 :fconst-2 :ldc :ldc-w
               :new :dup :dup-x1 :dup-x2])
      (--> +2 [:dconst-0 :dconst-1 :lconst-0 :lconst-1
               :ldc2-w :dup2 :dup2-x1 :dup2-x2])
      (--> +0 [:anewarray :newarray :arraylength])
      (--> -2 [:if-acmpeq :if-acmpne
               :if-icmpeq :if-icmpne
               :if-icmplt :if-icmpge
               :if-icmpgt :if-icmple])
      (--> -1 [:ifeq :ifne :iflt :ifge :ifgt :ifle
               :ifnonnull :ifnull])
      (--> -1 [:lookupswitch :tableswitch])
      (--> -1 [:monitorenter :monitorexit])
      (--> +1 [:jsr :jsr-w])
      (--> +0 [:goto :goto-w])
      (--> +0 [:nop :swap :iinc])
      (--> -1 [:iadd :idiv :imul :irem :isub
               :iand :ior :ixor :ishl :ishr :iushr
               :fadd :fdiv :fmul :frem :fsub])
      (--> -2 [:ladd :ldiv :lmul :lrem
               :land :lor :lxor])
      (--> -1 [:ishl :ishr :iushr
               :lshl :lshr :lushr]))))

(defn stack-delta
  "Number of words added to or removed from the operand stack
  as a result of this instruction. Called on first pass
  of the assembler."
  [op orig]
  (or (get +stack-delta+ op)
  (case op
    :getfield
    (let [[fspec] orig
          [_ _ tdesc] fspec]
      (if (#{:long :double} tdesc) 1 0))

    :getstatic
    (let [[fspec] orig
          [_ _ tdesc] fspec]
      (if (#{:long :double} tdesc) 2 1))

    :putfield
    (let [[fspec] orig
          [_ _ tdesc] fspec]
      (if (#{:long :double} tdesc) -3 -2))

    :putstatic
    (let [[fspec] orig
          [_ _ tdesc] fspec]
      (if (#{:long :double} tdesc) -2 -1))

    (:invokeinterface :invokevirtual :invokespecial)
    (let [[mspec] orig
          [_ _ mdesc] mspec
          [_ ret args] mdesc]
      (- (sizeof-desc ret)
         1 (reduce + (map sizeof-desc args))))

    :invokestatic
    (let [[mspec] orig
          [_ _ mdesc] mspec
          [_ ret args] mdesc]
      (- (sizeof-desc ret)
         (reduce + (map sizeof-desc args))))

    :multianewarray (let [[idx dim] orig]
                      (- (dec dim)))
    :wide
    (let [[wop] orig]
      (stack-delta wop orig)))))

(defn mdesc-args [[_ _ [_ _ args]]] args)

;; Convert Xload n <--> Xload_n, etc.
;; Do NOT convert bipush -> sipush
;; invokeinterface, wide, *switch need the original (symbolic) arguments
(defn emit-modify
  "Convert an instruction into shorter or longer format
  if possible or necessary. Called on first pass
  of the assembler."
  [{:keys [op args] :as ins} orig]
  (let [delta (stack-delta op orig)
        ins (assoc ins :stack delta)]
    (if (contains? +simple-instructions+ op)
      ins
      (case op
        (:aload :astore :iload :istore :lload :lstore
         :fload :fstore :dload :dstore)
        (let [[idx] args]
          (cond
            (<= idx 3) (struct instruction (load-store-op op idx) [] delta)
            (<= idx 0xFF) ins
            :else (struct instruction :wide [op idx] delta)))

        (:ldc :ldc-w)
        (let [[idx] args]
          (if (<= idx 0xFF)
            (struct instruction :ldc [idx] delta)
            (struct instruction :ldc-w [idx] delta)))

        :ret
        (let [[idx] args]
          (if (<= idx 0xFF)
            (struct instruction :ret [idx] delta)
            (struct instruction :wide [:ret idx] delta)))

        :iinc
        (let [[idx const] args]
          (if (and (<= idx 0xFF) (<= -0x80 const 0x7F))
            ins
            (struct instruction :wide [op idx const] delta)))

        ; calculate argument count including 'this'
        :invokeinterface
        (let [[idx] args
              iface-args (mdesc-args (first orig))]
          (struct instruction op
                  [idx (->> iface-args
                         (map sizeof-desc)
                         (reduce + 1)) 0]
                  delta))

        :lookupswitch
        (let [[_ default n & pairs] orig]
          (struct instruction op
                  (apply vector nil default n pairs)
                  delta))

        :tableswitch
        (let [[_ default low high & tab] orig]
          (struct instruction op
                  (apply vector nil default low high tab)
                  delta))

        :wide
        ins ; TODO: narrow

        (:anewarray :bipush :checkcast :getfield :getstatic
         :goto :goto-w :if-acmpeq :if-acmpne :if-icmpeq :if-icmpne
         :if-icmplt :if-icmpge :if-icmpgt :if-icmple
         :ifeq :ifne :iflt :ifge :ifgt :ifle
         :ifnonnull :ifnull :instanceof
         :invokedynamic :invokespecial :invokevirtual :invokestatic
         :jsr :jsr-w :ldc2-w :multianewarray :new :newarray
         :putfield :putstatic :sipush)
        ins
        ))))

(defn emit-instr
  "Add one instruction to the code buffer converting symbolic
  constants to constant pool indices"
  [cref mref instr ctx]
  (dosync
    (let [code (get-in @mref [:code])
          pool (:symtab @cref)
          currpc (:pc code)
          {:keys [op args]} instr
          atyp (next (+opcodes+ op))
          norm (map normalize-arg (concat atyp (repeat ::null)) args)
          {:keys [locals]} ctx ; TODO
          arg-m (with-monad state-m
                  (m-map (fn [[t a]]
                           (lookup-instr-arg t a ctx))
                         (map vector atyp norm)))]
      (let [[argidx ntab] (arg-m pool)
            ninstr (emit-modify (assoc instr :args argidx) norm)
            [nextpc ncode] ((domonad state-m
                              [nextpc (advance
                                        (ins-size currpc instr
                                                  (:labels code)))
                               _ (update-val :asm
                                   #(conj % ninstr))]
                              nextpc) code)]
        (alter mref assoc-in [:code] ncode)
        (alter cref assoc :symtab ntab)))))

; (label foo)
(defn label? [ins]
  (= (first ins) :label))

; (block start end [var type...]
;   body)
(defn block? [ins]
  (= (first ins) :block))

(defn emit1
  "Add an instruction, block, or label into the instruction stream"
  ([cref mref item] (emit1 cref mref (get-in @mref [:code :ctx]) item))
  ([cref mref ctx item]
   {:pre [(and (sequential? item) (not (empty? item)))]}
   (cond
     ;; Todo: LineNumberTable items [:line 128]
     ;;       block directive adds to LocalVarTable
     (label? item)
     (let [[_ lbl] item]
        (dosync
         (alter mref update-in [:code]
                (comp second
                  (domonad state-m
                    [_ (add-label lbl)
                     _ (update-val :asm #(conj % item))]
                    nil)))))

     (= (first item) 'do)
     (let [body (rest item)]
       (emit1 cref mref (apply vector
                              'block nil nil []
                              body)))

     (= (first item) :catch)
     (let [[_ beg end handler spec] item]
       (dosync
         (let [[idx tab]
               (if-not spec
                 [0 nil]
                 ((class-to-pool
                    (normalize-type-specifier spec))
                  (:symtab @cref)))]
           (when spec (alter cref assoc :symtab tab))
           (alter mref update-in [:code :exception-table]
                  conj {:start-pc beg :end-pc end
                        :handler-pc handler
                        :catch-type idx}))))

     (block? item)
     (let [[_ beg end vars & items] item]
       (dosync
         ;; Todo: generate LocalVarTable entries
         (when beg (emit1 cref mref [:label beg]))
         (let [subctx (merge-locals ctx vars)]
           (alter mref update-in [:code :max-locals]
                  (partial max (:size subctx)))
           (doseq [i items]
             (emit1 cref mref subctx i)))
         (when end (emit1 cref mref [:label end]))))

     :else
     (let [[op & args] item]
       (emit-instr cref mref (struct instruction op args nil) (:vars ctx))))))

(defn emit [cref mref items]
  (doseq [item items]
    (emit1 cref mref item)))

;;;; Second pass of the assembler
;; Calculate exact offset of each instruction and label;
;; transform short jumps into long jumps if necessary

(defn exact-size
  "Returns exact size in bytes of an instruction.
  Called on the second pass of the assembler."
  [{:keys [op args] :as ins} pc]
  (if (contains? +simple-instructions+ op)
    1
    (case op
      (:aload :astore :bipush :dload :dstore :fload :fstore :iload :istore
       :ldc :lload :lstore :newarray :ret)
      2
      (:anewarray :checkcast :getfield :getstatic :iinc :instanceof
       :invokedynamic :invokespecial :invokestatic :invokevirtual
       :ldc-w :ldc2-w :new :putfield :putstatic :sipush
       :goto :jsr)
      3
      (:multinewarray)
      4
      (:invokeinterface :jsr-w :goto-w)
      5

      (:if-acmpeq :if-acmpne :if-icmpeq :if-icmpne :if-icmplt
       :if-icmpge :if-icmpgt :if-icmple :ifeq :ifne :iflt :ifge
       :ifgt :ifle :ifnonnull :ifnull)
      3

      :lookupswitch
      (let [[_ default count] args 
            fix (+ 9 (* 8 count))
            pad (bin/padd4 (inc pc))]
        (+ fix pad))

      :tableswitch
      (let [[_ default low high] args
            fix (+ 13 (* 4 (inc (- high low))))
            pad (bin/padd4 (inc pc))]
        (+ fix pad))

      :wide
      (let [[wop] args]
        (if (= wop :iinc) 6 4)))))

(def +negate-op+
  (let [pairs {:if-acmpeq :if-acmpne,
               :if-icmpeq :if-icmpne,
               :if-icmplt :if-icmpge,
               :if-icmpgt :if-icmple,
               :ifeq :ifne, :iflt :ifge, :ifgt :ifle,
               :ifnonnull :ifnull}]
    (into pairs (bin/invert-map pairs)))) ; symmetrize

(defn jump-target [labels target]
  (if (integer? target)
    target
    (first (labels target))))

;; Transform if* --> ifnot* goto-w,
;;           goto --> goto-w
;; if necessary
(defn refine-split
  "Widen a jump instruction if necessary.
  Called on second pass of the assembler."
  [{:keys [op args] :as ins} pc labels]
  (case op
    (:if-acmpeq :if-acmpne :if-icmpeq :if-icmpne :if-icmplt
     :if-icmpge :if-icmpgt :if-icmple :ifeq :ifne :iflt :ifge
     :ifgt :ifle :ifnonnull :ifnull)
    (let [[target] args]
      (if (integer? target)
        [ins]
        (if-let [target-label (labels target)]
          (if (bound-interval? [pc pc] target-label [-0x8000 0x7FFF])
            [ins]
            [(struct instruction (get +negate-op+ op) [8] (:stack ins))
             (struct instruction :goto-w [target] 0)])
          (throw (IllegalArgumentException.
                   (str "Undefined label " target))))))
    
    (:goto :goto-w)
    (let [[target] args]
      (if (integer? target)
        [ins]
        (if-let [target-label (labels target)]
          (if (bound-interval? [pc pc] target-label [-0x8000 0x7FFF])
            [(assoc ins :op :goto)]
            [(assoc ins :op :goto-w)])
          (throw (IllegalArgumentException.
                   (str "Undefined label " target))))))

    ; default
    [ins]))

(defn refine1
  "Calculate exact offset and size for one label or instruction.
  Called on second pass of the assembler."
  [ins]
  (if (label? ins)
    ;; Remove the label from instruction stream
    (let [[_ lbl] ins]
      (fn [{:keys [labels pc] :as code}]
        [nil (assoc-in code [:labels lbl] [pc pc])]))
    (fn [{:keys [labels pc] :as code}]
      (let [split (refine-split ins pc labels)]
        (loop [target (:asm code) pc pc remain split]
          (if (seq remain)
            (let [ins (first remain)
                  siz (exact-size ins pc)]
              (recur (assoc target pc ins)
                     (+ pc siz)
                     (next remain)))
            [nil (-> code (assoc-in [:pc] pc)
                          (assoc-in [:asm] target))]))))))

(defn refine
  "Calculate exact offsets for all instructions and labels in a method"
  [meth]
  (dosync
    (alter meth
           update-in [:code]
           (comp second
             (domonad state-m
               [_   (set-val :pc 0)
                ins (set-val :asm (sorted-map))
                _   (m-map refine1 ins)
                siz (fetch-val :pc)
                _   (set-val :code-length siz)]
               nil)))))

;;;; Third pass: control & data flow analysis
;; Calculate stack size and StackMapTable

(def *java-hierarchy*
  (-> (make-hierarchy)
      (derive :one-word :top)
      (derive :two-word :top)
      (derive :reference :one-word)
      (derive Object :reference)
      (derive :uninitialized :reference)
      (derive :uninitialized-this :uninitialized)
      (derive :null :reference)
      (derive :long :two-word)
      (derive :double :two-word)
      (derive :int :one-word)
      (derive :float :one-word)))

(defn array-class [^Class c]
  (class (make-array c 0)))

;; brute force
(defn ancestors+ [x]
  (let [anc (ancestors *java-hierarchy* x)]
    (if (and (class? x)
             (.isArray ^Class x))
      (->> (.getComponentType ^Class x) ancestors+
           (filter class?) (map array-class)
           (into anc))
      anc)))

;; handles array class covariance
(defn j-isa? [x y]
  (or (isa? *java-hierarchy* x y)
      (and (isa? *java-hierarchy* y :reference)
           (= x :null))
      (and (class? x) (class? y)
           (let [^Class x x ^Class y y]
             (.isAssignableFrom y x)))))

;             (and (.isArray x) (.isArray y)
;                  (j-isa? (.getComponentType x)
;                          (.getComponentType y)))))))

(defn unify1 [l r]
  (cond
    (and (set? l) (set? r))
    (clojure.set/intersection l r)

    (set? r)
    (if (contains? r l) l
      (clojure.set/intersection
        (ancestors+ l) r))

    (set? l)
    (if (contains? l r) r
      (clojure.set/intersection
        (ancestors+ r) l))

    (j-isa? l r) r
    (j-isa? r l) l

    :else
    (clojure.set/intersection
      (ancestors+ l)
      (ancestors+ r))))

(defn unify-frame [pc [loc stak :as curr]]
  (fn [code]
    (let [[ploc pstak :as prev] (get (:frames code) pc)]
      (if prev
        [nil (assoc-in [code :frames pc]
                       [(map unify1 loc ploc) (map unify1 stak pstak)])]
        [nil (assoc-in [code :frames pc] curr)]))))

;; Basic blocks graph

(defn target-offset [target pc labels]
  (if (integer? target)
    (+ pc target)
    (first (get labels target))))

(defn next-block [blocks b]
  (ffirst (subseq blocks > b)))

(defn block-code [asm blocks b]
  (if-let [nxt (next-block blocks b)]
    (subseq asm >= b < nxt)
    (subseq asm >= b)))

(def +jump-ops+ #{:goto :goto-w :jsr :jsr-w})
(def +branch-ops+ #{:if-acmpeq :if-acmpne :if-icmpeq :if-icmpne
                    :if-icmplt :if-icmpge :if-icmpgt :if-icmple
                    :ifeq :ifne :iflt :ifge :ifgt :ifle
                    :ifnonnull :ifnull})
(def +break-ops+ #{:athrow :return :areturn :ireturn
                   :freturn :lreturn :dreturn})

;; Assume valid bytecode: last block must end in a jump, break or switch
(defn block-neighbors [asm blocks labels b]
  (let [nxt (next-block blocks b)
        [pc {op :op [target & more] :args :as last-instr}]
        (first (if nxt
                 (rsubseq asm >= b < nxt)
                 (rseq asm)))]
    (cond
      (+jump-ops+ op)
      #{(jump-target labels target)}

      (+break-ops+ op)
      #{}

      (+branch-ops+ op)
      #{nxt (jump-target labels target)}

      (= op :tableswitch)
      (let [[default min max & targets] more]
        (apply set (jump-target labels default)
               (map (partial jump-target labels) targets)))

      (= op :lookupswitch)
      (let [[default num & pairs] more]
        (apply set (jump-target labels default)
               (map (partial jump-target labels)
                    (take-nth 2 (next pairs)))))

      :else
      #{nxt})))

(defn basic-blocks
  "Returns the sorted set of offsets at the start of the basic blocks"
  [code labels]
  (loop [blocks (sorted-map) ins (seq code) start? true]
    (if-not ins
      blocks
      (let [[pc {:keys [op args] :as whole}] (first ins)
            blocks (if start? (assoc blocks pc {}) blocks)]
        (case op
          (:areturn :athrow :dreturn :freturn :ireturn :lreturn :return)
          (recur blocks (next ins) true)

          (:goto :goto-w :if-acmpeq :if-acmpne
           :if-icmpeq :if-icmpne :if-icmplt :if-icmpge
           :if-icmpgt :if-icmple :ifeq :ifne :iflt :ifge
           :ifgt :ifle :ifnonnull :ifnull)
          (recur (assoc blocks (target-offset (first args) pc labels) {})
                 (next ins) true)

          :lookupswitch
          (let [[_ default count & pairs] args]
            (recur (apply assoc blocks
                          (target-offset default pc labels) {}
                          (interleave
                            (map #(target-offset % pc labels)
                                 (take-nth 2 (rest pairs)))
                            (repeat {})))
                   (next ins) true))

          :tableswitch
          (let [[ _ default low high & tab] args]
            (recur (apply assoc blocks
                          (target-offset default pc labels) {}
                          (interleave
                            (map #(target-offset % pc labels) tab)
                            (repeat {})))
                   (next ins) true))

          ; (:jsr jsr-w :ret) -- unimplemented

          ; default
          (recur blocks (next ins) false))))))

(defn map-keys [f m]
  (into (sorted-map)
        (map vector (keys m) (map f (keys m)))))

(defn block-graph
  "Given the set of start offsets, calculates the
  directed control flow graph of basic blocks"
  [blocks code]
  (let [all-neighbors
        (map-keys (partial block-neighbors
                           (:asm code)
                           blocks
                           (:labels code))
                  blocks)]
    (struct graph/directed-graph (keys all-neighbors) all-neighbors)))

(defn block-stack-size [code stack-in]
  (reduce (fn [[curr high] {delta :stack}]
            (let [nxt (+ curr delta)]
              [nxt (max high nxt)]))
          [stack-in stack-in]
          (vals code)))

#_(require 'clojure.set)
(defn method-stack-size [block-graph code labels extab]
  (let [init (into {0 0}
                   (for [{handler :handler-pc} extab]
                     [(get labels handler) 1]))
        calc (with-monad state-m
               (m-map #(fn [stacks]
                         (let [[out high]
                               (block-stack-size
                                 (block-code code (:neighbors block-graph) %)
                                 (stacks %))]
                           #_(doseq [node (graph/get-neighbors block-graph %)]
                             (when (and (contains? stacks node)
                                        (not= (stacks node) out))
                               (println "Stack mismatch" % "/" out
                                        "-->" node "/" (stacks node))))
                           [high (merge stacks
                                   (zipmap (graph/get-neighbors block-graph %)
                                           (repeat out)))]))
                      (graph/lazy-walk block-graph (keys init) #{})))
        [siz exit] (calc init)]
    #_(let [dead (clojure.set/difference
                 (set (:nodes block-graph))
                 (set (keys exit)))]
      (when (seq dead)
        (println "Dead code:" dead)))
    (reduce max siz)))

(defn asm1 
  "Assemble one bytecode instruction"
  [^ByteBuffer buf {:keys [op args]} labels]
  (let [pc (.position buf)
        [b & atyps] (+opcodes+ op)]
    (bin/write-binary ::bin/uint8 buf b)
    (doseq [[t a] (map vector atyps (concat args (repeat 0)))]
      (if (symbol? a)
        (bin/write-binary t buf (- (first (get labels a)) pc))
        (bin/write-binary t buf a)))
    (case op
      :tableswitch
      (let [[_ default low high & more] args]
        (doseq [targ more]
          (if (symbol? targ)
            (bin/write-binary ::bin/int32 buf (- (first (get labels targ)) pc))
            (bin/write-binary ::bin/int32 buf targ))))

      :lookupswitch
      (let [[_ default npairs & more] args]
        (doseq [[key targ] (partition 2 more)]
          (bin/write-binary ::bin/int32 buf key)
          (if (symbol? targ)
            (bin/write-binary ::bin/int32 buf (- (first (get labels targ)) pc))
            (bin/write-binary ::bin/int32 buf targ))))

;      :wide
;      (let [[wop & watyps]
;            (+lookup-wide+ (bin/read-binary ::bin/uint8 buf))
;            wargs (doall (map #(lookup-pool %
;                                 (bin/read-binary % buf))
;                              watyps))]
;        (apply vector pc op wop wargs))

      ; default
      nil)))

(defn doasm
  "Assemble the bytecode array of a method"
  [ins labels len]
  (let [bytecode (byte-array len)]
    (let [^ByteBuffer buf (ByteBuffer/wrap bytecode)]
      (doseq [[offs instr] ins] 
        (asm1 buf instr labels)))
    bytecode))
;[offs instr]
;;;; Top-level interface

(defn- get-or-die [map val]
  (or (get map val) (throw (Exception. (str "Undefined label " val)))))

;; refine, compute block graph, TODO: generate stack map
(defn assemble-method [cref mref]
  (dosync
    (when-not (some #{:abstract :native} (:flags @mref))
      (refine mref)
      (let [code (:code @mref)
            labels (into {} (for [[k [v v]] (:labels code)] [k v]))
            extab (:exception-table code)
            blocks (basic-blocks (:asm code) (:labels code))
            graph (block-graph blocks code)
            stack (method-stack-size graph (:asm code) labels extab)]
        (alter mref update-in [:code :exception-table]
               (fn [xs]
                 (vec (map (fn [{:keys [start-pc end-pc
                                        handler-pc catch-type]}]
                             {:start-pc (get-or-die labels start-pc)
                              :end-pc (get-or-die labels end-pc)
                              :handler-pc (get-or-die labels handler-pc)
                              :catch-type catch-type})
                           xs))))
        (let [code (ref (assoc (:code @mref) :max-stack stack))]
          (when-let [lns (seq (map (fn [{:keys [line start-pc]}]
                                     {:line line
                                      :start-pc (labels start-pc)})
                                   (:line-numbers @code)))]
            (add-attribute cref code {:name "LineNumberTable"
                                      :table (vec lns)}))
          (add-attribute cref mref @code))))))

(defn assemble-class [cref]
  (dosync
    (let [cls @cref]
      (assoc cls
             :methods (map deref (:methods cls))
             :fields (map deref (:fields cls))))))

(defmacro assembling [bindings & body]
  (let [syms (take-nth 2 bindings)
        vals (take-nth 2 (rest bindings))
        refs (map (fn [expr]
                    `(ref (init-class (merge empty-class ~expr))))
                  vals)
        names (map :name vals)]
    `(let ~(vec (interleave syms refs))
       (binding [*assembling* (assoc *assembling*
                                     ~@(interleave names syms))]
         ~@body))))

;; Walk the class description trees
;; calling add-field, add-method, emit, assemble-method, assemble-class
(defn assemble [cs]
  cs)

(defn sendout
  ([cls] (sendout cls *compile-path*))
  ([cls path]
   (let [buf (ByteBuffer/allocate 100000)
         qname (str (:name cls))
         sep (System/getProperty "file.separator")
         fpath (apply str (interpose sep (.split qname "\\.")))
         cls-file (java.io.File. path (str fpath ".class"))]
     (.mkdirs (java.io.File. (.getParent cls-file)))
     (println "Writing to" (.getCanonicalPath cls-file))
     (with-open [fos (java.io.FileOutputStream. cls-file)
                 chan (.getChannel fos)]
       (bin/write-binary ::ClassFile buf cls)
       (.flip buf)
       (.write chan buf)))))

(defn readout [cls]
  (let [buf (ByteBuffer/allocate 10000)]
    (bin/write-binary ::ClassFile buf cls)
    (.flip buf)
    (bin/read-binary ::ClassFile buf)))

;;;; Debug: inspect classes being generated in assembling

#_(defn inspect [cref]
  (clojure.inspector/inspect-tree
    (assemble-class cref)))
