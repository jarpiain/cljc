;; Copyright (c) Juha Arpiainen. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; Based on clojure/lang/Compiler.java
;; of clojure-1.3.0-alpha1

(ns org.subluminal.compiler
  (:refer-clojure :exclude [load load-file compile eval require])
  (:import (java.io Reader InputStreamReader File FileOutputStream)
           (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.nio.charset Charset)
           (java.util IdentityHashMap Arrays List)
           (java.lang.reflect Field Method Constructor)
           (clojure.lang LineNumberingPushbackReader
                         DynamicClassLoader Reflector LazySeq IObj
                         RestFn AFunction Namespace Associative
                         IPersistentList IPersistentVector
                         IPersistentMap IPersistentSet
                         PersistentList PersistentList$EmptyList
                         PersistentVector PersistentArrayMap
                         PersistentHashSet PersistentHashMap
                         RT Util Var Symbol Keyword ISeq IFn))
  (:use (clojure.contrib monads)
        (clojure [inspector :only [inspect-tree]])
        (org.subluminal util))
  (:require (org.subluminal [class-file :as asm] [binfmt :as bin])))

(declare syncat)

(defmulti analyze
  "Parse a form into an AST of nested maps. The resulting map
  will have at least the ::etype and :gen-type keys

  Position should be one of :expression, :statement, :return, or :eval.

  The type request argument should be set to void if the value
  will be discarded. If a primitive result is permitted, type-req
  should be set to true or a specific primitive Class object but
  the analyzed form need not have the requested primitive :gen-type.
  If type-req is nil, a boxed result is always generated.
  In any case the analyze-contract predicate is satisfied
  between the requested and actual type.

  The name argument is used to name the class generated from
  a fn* form."
  (fn [position type-req name form]
    (syncat form))
  :default ::invocation)

(defmulti gen
  "Generate bytecode from an analyzed form.
  The generated code will leave an item of
  type :gen-type on the operand stack
  (or nothing if :gen-type is void)"
  ::etype :default ::invalid)

(defmulti eval-toplevel
  "Evaluate an analyzed top-level form"
  (fn [form ^ClassLoader loader] (::etype form))
  :default ::invalid)

(defmethod gen ::invalid
  [form]
  (throw (IllegalArgumentException.
           (str "gen: invalid form " form))))

(defmethod eval-toplevel ::invalid
  [form _]
  (throw (Exception. (str "Can't eval " (::etype form) " form"))))

(clojure.core/load "cljc/util")
(clojure.core/load "cljc/types")
(clojure.core/load "cljc/atomics")
(clojure.core/load "cljc/compound")
(clojure.core/load "cljc/object")

(comment
(defstruct context
           :loader
           :position    ; :eval :statement :expression :return
           :lexicals    ; symbol -> struct lex-binding
           :loop-locals ; vector lex-binding
           :loop-label  ; symbol
           :method      ; gensym --> index
           :fn          ; gensym --> index
           :catching    ; catch-finally

           :clear-tag   ; gensym - identity
           :clear-root  ; context
           :clear-kind  ; :branch :path
           :clear-path  ; seq of context
           :binding-sites ; map of label -> lex-binding-inst
           :index
           :parent)     ; context
)

(def null-context
  (let [t (gensym "NULL")]
    {:position :eval
     :lexicals {}
     :loop-locals []
     :loop-label nil
     :method nil
     :fn nil
     :index {}
     :catching nil
     :clear-tag t
     :clear-root t
     :clear-kind :path
     :binding-sites {}
     :parent nil}))

(comment
(defstruct fn-class
           :symbol ; may be nil
           :internal-name
           :enclosing-method
           :closed-lexicals
           :constants
           :context)

(defstruct method ; metadata on method form
           :params)

(defstruct lex-binding
           :symbol ; 'x
           :label  ; (gensym)
           :kind   ; :fnarg :local :closed
           :method-tag ; gensym, relevant for :local :fnarg
           :closed-idx
           :java-type
           :fn-tag
           :clear-root ; gensym
           ;; for binding instances
           :live
           :clear-path)
)

(defn syncat
  "The syntax category of a form. Dispatch function for analyze."
  [x]
  (let [x (if (instance? LazySeq x)
            (if-let [sx (seq x)] sx ())
            x)]
    (cond
      (nil? x) ::null
      (or (true? x) (false? x)) ::boolean
      (symbol? x) ::symbol
      (number? x) ::number
      (string? x) ::string
      (keyword? x) ::keyword
      (and (coll? x) (empty? x)) ::empty
      (seq? x) [::special (first x)]
      (vector? x) ::vector
      (map? x) ::map
      (set? x) ::set
      :else ::constant)))

(declare gen-constant)
(defn gen-constant-array [objects]
  `([:ldc ~(int (count objects))]
    [:anewarray ~Object]
    ~@(mapcat (fn [obj i]
                `([:dup]
                  [:ldc ~(int i)]
                  ~@(gen-constant obj)
                  [:aastore]))
              objects (range (count objects)))))

;;;; Debugging

(def
  ^{:doc "If true, the compiler will open each compiled class
         file into a tree inspector"
    :dynamic true}
  *debug-inspect* false)

(defn analyze-fragment "Test analysis"
  ([form] (analyze-fragment form :expression nil))
  ([form pos typ] (analyze-fragment form pos typ (DynamicClassLoader.)))
  ([form pos typ loader]
   (let [[res ctx]
         (run-with (assoc null-context :loader loader)
           [_ (push-object-frame {:name 'toplevel})
            a (analyze pos typ nil form)
            _ pop-frame]
           a)]
     res)))

(defn gen-fragment "Test gen"
  ([form] (gen (analyze-fragment form)))
  ([form pos] (gen (analyze-fragment form pos nil)))
  ([form pos typ] (gen (analyze-fragment form pos typ))))

(defn eval
  [form]
  (let [ldr (DynamicClassLoader.)]
    (eval-toplevel (analyze-fragment form :eval nil ldr) ldr)))

#_(defmethod analyze [::special 'set!]
  [[_ target value :as form]]
  (if (not= (count form) 3)
    (throw (IllegalArgumentException.
             "Malformed assignment, expecting (set! target val)"))
    (run
      [pos (set-val :position :expression)
       target (analyze target)
       value (analyze value)
       _ (set-val :position pos)]
      (if (isa? (::etype target) ::assignable)
        (with-meta `(~'set! ~target ~value)
                   {::etype ::set!
                    :position pos})
        (throw (IllegalArgumentException. "Invalid assignment target"))))))

;; Should change to defmulti
;; TODO serialization with print/read
(defn gen-constant
  "Generated bytecode to recreate a (quoted) object
  at class initialization time"
  [c]
  (cond
    (nil? c)
    `([:aconst-null])
    (instance? Boolean c)
    (if c
      `([:getstatic ~[Boolean 'TRUE Boolean]])
      `([:getstatic ~[Boolean 'FALSE Boolean]]))
    (instance? Integer c)
    `([:ldc ~c]
      [:invokestatic ~[Integer 'valueOf [:method Integer [:int]]]])
    (instance? Long c)
    `([:ldc2-w ~c]
      [:invokestatic ~[Long 'valueOf [:method Long [:long]]]])
    (instance? Double c)
    `([:ldc2-w ~c]
      [:invokestatic ~[Double 'valueOf [:method Double [:double]]]])
    (string? c)
    `([:ldc ~c])
    (instance? Character c)
    `([:ldc ~(int c)]
      [:invokestatic ~[Character 'valueOf [:method Character [:char]]]])

    (class? c)
    (if (.isPrimitive ^Class c)
      (cond
        (= c Boolean/TYPE) `([:getstatic ~[Boolean 'TYPE Class]])
        (= c Byte/TYPE)    `([:getstatic ~[Byte 'TYPE Class]])
        (= c Short/TYPE)   `([:getstatic ~[Short 'TYPE Class]])
        (= c Integer/TYPE) `([:getstatic ~[Integer 'TYPE Class]])
        (= c Long/TYPE)    `([:getstatic ~[Long 'TYPE Class]])
        (= c Float/TYPE)   `([:getstatic ~[Float 'TYPE Class]])
        (= c Double/TYPE)  `([:getstatic ~[Double 'TYPE Class]])
        (= c Void/TYPE)    `([:getstatic ~[Void 'TYPE Class]]))
      `([:ldc ~(.getName c)]
        [:invokestatic ~[Class 'forName [:method Class [String]]]]))

    (symbol? c)
    `(~(if (namespace c)
         [:ldc (namespace c)]
         [:aconst-null])
      [:ldc ~(name c)]
      [:invokestatic ~[Symbol 'intern [:method Symbol [String String]]]])

    (keyword? c)
    `(~(if (namespace c)
         [:ldc (namespace c)]
         [:aconst-null])
      [:ldc ~(name c)]
      [:invokestatic ~[Keyword 'intern [:method Keyword [String String]]]])

    (var? c)
    `([:ldc ~(str (.name (.ns c)))]
      [:ldc ~(str (.sym c))]
      [:invokestatic ~[RT 'var [:method Var [String String]]]])

    (set? c) 
    `(~@(gen-constant-array c)
      [:invokestatic
       ~[RT 'set [:method IPersistentSet [[:array Object]]]]])

    (map? c)
    `(~@(gen-constant-array (apply concat (seq c)))
      [:invokestatic
       ~[RT 'map [:method IPersistentMap [[:array Object]]]]])

    (or (seq? c) (list? c))
    `(~@(gen-constant-array c)
      [:invokestatic
       ~[Arrays 'asList [:method List [[:array Object]]]]]
      [:invokestatic
       ~[PersistentList 'create [:method IPersistentList [List]]]])

    (vector? c)
    `(~@(gen-constant-array c)
      [:invokestatic
       ~[RT 'vector [:method IPersistentVector [[:array Object]]]]])
    :else
    (let [cstr (binding [*print-dup* true]
                 (RT/printString c))]
      `([:ldc ~cstr]
        [:invokestatic ~[RT 'readString [:method Object [String]]]]
        [:checkcast ~(class c)]))))

(defn emit-constants [obj cref mref]
  (doseq [{:keys [cfield source-type obj lit-val]} (:constants obj)]
    (asm/emit cref mref
      `(~@(gen-constant lit-val)
        [:putstatic [~obj ~cfield ~source-type]]))))

;; TODO: defmulti --> deftypes also
(defn emit-methods [{:keys [variadic-arity] :as obj} cref]
  (when variadic-arity
    (let [mref (asm/add-method cref
                 {:name 'getRequiredArity
                  :descriptor [:method :int []]
                  :flags #{:public}})]
      (asm/emit cref mref
        `([:sipush ~(int variadic-arity)]
          [:ireturn]))
      (asm/assemble-method cref mref)))
  (doseq [{:keys [argv line bind this loop-label body] :as mm} (:methods obj)]
    (let [variadic? (variadic? argv)
          mref (asm/add-method cref
                   {:name (if variadic? 'doInvoke 'invoke)
                    :descriptor [:method Object
                                 (repeat (count bind) Object)]
                    :params (map :label bind)
                    :flags #{:public}
                    :throws [Exception]})]
      (asm/emit1 cref mref [:label loop-label])
      (when line
        (asm/emit1 cref mref [:line line loop-label]))
      (let [body (gen body)]
        (asm/emit cref mref body))
      (asm/emit1 cref mref [:areturn])
      (asm/assemble-method cref mref))))

(defn write-class-file [cname buf]
  (let [cfile (File. (str *compile-path* File/separator
                          (.replace (str cname) "." File/separator)
                          ".class"))
        dir (.getParentFile cfile)]
    (.mkdirs dir)
    (with-open [fos (FileOutputStream. cfile)
                chan (.getChannel fos)]
      (.truncate chan 0)
      (.write chan buf))))

(defn compile-class
  "Compile and load the class representing
  a fn or (not implemented yet) deftype"
  [ctx obj super ifaces]
  (asm/assembling [c {:name (:name obj)
                      :extends super
                      :implements ifaces
                      :source-file (or (:source-file ctx) "REPL")
                      :flags #{:public :super :final}}]
    ;; static fields for constants
    (doseq [fld (:constants obj)]
      (let [{:keys [cfield source-type]} fld]
        (asm/add-field c {:name cfield
                          :descriptor source-type
                          :flags #{:public :final :static}})))

    ;; static init for constants, keywords and vars
    (let [clinit (asm/add-method c {:name '<clinit>
                                    :descriptor [:method :void []]
                                    :flags #{:public :static}})]
      (emit-constants obj c clinit)
      (asm/emit1 c clinit [:return])
      (asm/assemble-method c clinit))

    ;; instance fields for closed-overs
    (doseq [[sym bind] (:closed-lexicals obj)]
      (let [{:keys [source-type]} bind]
        (asm/add-field c {:name sym
                          :descriptor source-type
                          :flags #{:public :final}})))

    (let [clos (:closed-lexicals obj)
          clos-names (keys clos)
          clos-types (map :source-type (vals clos))]
      ;; ctor that takes closed-overs and inits base + fields
      ;; TODO: ctors that take metadata, deftype ctors
      (let [init (asm/add-method c {:name '<init>
                                    :descriptor [:method :void clos-types]
                                    :params clos-names
                                    :flags #{:public}})]
        (asm/emit c init
          `([:aload-0]
            [:invokespecial ~[super '<init> [:method :void []]]]
            ~@(mapcat (fn [arg typ]
                        `([:aload-0]
                          ~[(load-op typ) arg]
                          [:putfield [~(:name obj) ~arg ~typ]]))
                      clos-names clos-types)
            [:return]))
        (asm/assemble-method c init)))

    (let [meta (asm/add-method c {:name 'meta
                                  :descriptor [:method IPersistentMap []]
                                  :flags #{:public}})]
      (asm/emit c meta
        `([:aconst-null]
          [:areturn]))
      (asm/assemble-method c meta))

    (let [with-meta (asm/add-method c {:name 'withMeta
                                       :descriptor
                                         [:method IObj [IPersistentMap]]
                                       :flags #{:public}})]
      (asm/emit c with-meta
        `([:aload-0]
          [:areturn]))
      (asm/assemble-method c with-meta))

    (emit-methods obj c)

    (let [c (asm/assemble-class c)
          bytecode (ByteBuffer/allocate (asm/class-length c))] 
      (bin/write-binary ::asm/ClassFile bytecode c)
      (let [arr (.array bytecode)]
        (try
          (.defineClass (:loader ctx) (str (:name obj)) arr nil)
          (when *compile-files*
            (.flip bytecode)
            (write-class-file (:name obj) bytecode))
          (catch Throwable e
            (println "Caught while loading class:" (class e) e))
          (finally
            (when *debug-inspect*
              (.clear bytecode)
              (inspect-tree (bin/read-binary ::asm/ClassFile bytecode)))))))))

;;;; toplevel

(defn form-seq [rd]
  (let [eof (Object.)
        pbr (if (instance? LineNumberingPushbackReader rd) rd
              (LineNumberingPushbackReader. rd))
        seqrd (fn seqrd [rd]
                (lazy-seq
                  (let [form (read rd nil eof)]
                    (when-not (identical? form eof)
                      (cons form (seqrd rd))))))]
    (seqrd pbr)))

(declare compile1)
(defn compile-file
  ([rd src-path src-name]
   (asm/assembling [ns-init {:name (file->class-name src-path RT/LOADER_SUFFIX)
                             :flags #{:public :super}
                             :source-file src-name}]
     (let [loader (DynamicClassLoader.)]
       (run-with (assoc null-context
                        :loader loader
                        :source-file src-name)
         [_ (push-object-frame {:name (symbol (file->class-name src-path RT/LOADER_SUFFIX))})
          _ (set-val :position :eval)
          context (set-state null-context)]
         (loop [ctx context loader-code [] forms (form-seq rd)]
           ;; Make sure to eval the form before reading the next one
           ;; since rebinding *ns* affects the reading of :: and `
           (if (seq forms)
             (let [form (first forms)
                   [code ctx] ((compile1 form) ctx)]
               (recur ctx (concat loader-code code) (next forms)))
             (do
               (let [clj-init (asm/add-method ns-init
                                              {:name 'load
                                               :flags #{:public :static}
                                               :descriptor [:method :void []]})]
                 (asm/emit ns-init clj-init loader-code)
                 (asm/emit1 ns-init clj-init [:return])
                 (asm/assemble-method ns-init clj-init))

               (let [[top _] (current-object ctx)]
                 (doseq [fld (:constants top)]
                   (let [{:keys [cfield source-type]} fld]
                     (asm/add-field ns-init {:name cfield
                                             :descriptor source-type
                                             :flags #{:public :final :static}})))
           ;; TODO inits in blocks of 100...
                 (let [init-const (asm/add-method ns-init
                                                  {:name '__init0
                                                   :descriptor [:method :void []]
                                                   :flags #{:public :static}})]
                   (emit-constants top ns-init init-const)
                   (asm/emit1 ns-init init-const [:return])
                   (asm/assemble-method ns-init init-const))

                 (let [clinit (asm/add-method ns-init
                                        {:name '<clinit>
                                         :descriptor [:method :void []]
                                         :flags #{:public :static}})
                       [start-try end-try end final]
                       (map gensym ["TRY__" "DONE__" "END__" "FIN__"])]
                   (asm/emit ns-init clinit
                     `([:invokestatic ~[(:name @ns-init) '__init0
                                          [:method :void []]]]
                       [:iconst-2] ; inlined Compiler/pushNS()
                       [:anewarray ~Object]
                       [:dup]
                       [:iconst-0] ; arr arr 0
                       [:ldc "clojure.core"]
                       [:invokestatic ~[Symbol 'intern
                                        [:method Symbol [String]]]]
                       [:ldc "*ns*"]
                       [:invokestatic ~[Symbol 'intern
                                        [:method Symbol [String]]]]
                       [:invokestatic ~[Var 'intern
                                        [:method Var [Symbol Symbol]]]]
                       [:aastore] ; arr[var,null]
                       [:invokestatic ~[PersistentHashMap 'create
                                        [:method PersistentHashMap
                                         [[:array Object]]]]]
                       [:invokestatic [~Var ~'pushThreadBindings
                                       [:method :void [~Associative]]]]

                       [:label ~start-try]
                       [:invokestatic ~[(:name @ns-init) 'load [:method :void []]]]
                       [:label ~end-try]
                       [:invokestatic [~Var ~'popThreadBindings [:method :void []]]]
                       [:goto ~end]
                       [:label ~final]
                       [:invokestatic [~Var ~'popThreadBindings [:method :void []]]]
                       [:athrow]
                       [:label ~end]
                       [:return]
                       [:catch ~start-try ~end-try ~final nil]))
                   (asm/assemble-method ns-init clinit))
                 (when *compile-files*
                   (let [c (asm/assemble-class ns-init)
                         bytecode (ByteBuffer/allocate (asm/class-length c))] 
                     (bin/write-binary ::asm/ClassFile bytecode c)
                     (.flip bytecode)
                     (write-class-file (:name @ns-init) bytecode))))))))))))

(defn- compile1
  "Compile and eval a top-level form"
  [form]
  (run [form (macroexpand-impl *ns* form)
        res (if (and (seq? form) (= (first form) 'do))
              (s-map compile1 (next form))
              (run [analyzed (analyze :eval nil nil form)
                    loader (fetch-val :loader)]
                (let [bytecode (gen analyzed)]
                  (eval-toplevel analyzed loader)
                  (list bytecode))))]
    (apply concat res)))

(defn- root-resource [lib]
  (str \/
       (.. (name lib)
           (replace \- \_)
           (replace \. \/))))

(defn load-lib
  [lib write-files?]
  (let [root (root-resource lib)
        cljfile (.substring (str root ".clj") 1)
        loader (RT/baseLoader)]
    (with-open [ins (.getResourceAsStream loader cljfile)
                inrd (InputStreamReader. ins (Charset/forName "UTF-8"))]
      (binding [*ns* *ns*
                *compile-files* write-files?]
        (compile-file inrd cljfile
          (.substring cljfile (inc (.lastIndexOf cljfile "/")))))))
  nil)

(defn compile
  "Compile and load a lib and write generated class files to *compile-path*"
  [lib]
  (load-lib lib true))

(defn require
  "Compile and load a lib"
  [lib]
  (load-lib lib false))

(defn load-file
  "A simple version to demonstrate bootstrapping"
  [filename]
  (let [loader (RT/baseLoader)]
    (with-open [ins (.getResourceAsStream loader filename)
                inrd (InputStreamReader. ins (Charset/forName "UTF-8"))]
      (binding [*ns* *ns*]
        (compile-file inrd filename
          (.substring filename (inc (.lastIndexOf filename "/"))))))))
