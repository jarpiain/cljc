(ns org.subluminal.compiler
  (:refer-clojure :exclude [compile eval])
  (:import (java.io Reader InputStreamReader)
           (java.nio ByteBuffer)
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
                         RT Var Symbol Keyword ISeq IFn))
  (:use (clojure.contrib monads)
        (clojure [inspector :only [inspect-tree]])
        (org.subluminal util))
  (:require (org.subluminal [class-file :as asm] [binfmt :as bin])))

(def gen nil)
(def analyze nil)
(def eval-toplevel nil)
(def *debug-inspect* false)

(declare syncat)

(defmulti analyze
  "Parse a form into an AST of nested maps. The resulting map
  will have at least the ::etype and :gen-type keys

  Position should be one of :expression, :statement, :return,
  or :eval. The :gen-type of a form analyzed in :statement
  position will always be void.
  
  If a primitive result is permitted, the type request argument
  should be set to true or a specific primitive Class object.
  The analyzed form need no have the requested :gen-type.
  If type-req is nil, a boxed result is always generated.
  In any case the analyze-contract predicate is satisfied
  between the requested and actual type.

  The name argument is used to name the class generated from
  a fn* form."
  (fn [position type-req name form] (syncat form))
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

(load "cljc/util")
(load "cljc/atomics")
(load "cljc/compound")
(load "cljc/object")

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

;;;; Primitives/boxing
(defn return-op [jt]
  (cond
    (= jt Long/TYPE) :lreturn
    (= jt Double/TYPE) :dreturn
    :else :areturn))

(defn gen-array [objects]
  `([:ldc ~(int (count objects))]
    [:anewarray ~Object]
    ~@(mapcat (fn [obj i]
                `([:dup]
                  [:ldc ~(int i)]
                  ~@(gen obj)
                  [:aastore]))
              objects (range (count objects)))))

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

;;;; synchronization

(defmethod analyze [::special 'monitor-enter]
  [[_ lockee]]
  (run
    [pos (set-val :position :expression)
     jt (set-val :want-unboxed nil)
     lockee (analyze lockee)
     _ (set-val :position pos)]
    {::etype ::monitor-enter
     :lock lockee
     :java-type Void/TYPE
     :position pos}))

(defmethod analyze [::special 'monitor-exit]
  [[_ lockee]]
  (run
    [pos (set-val :position :expression)
     jt (set-val :want-unboxed nil)
     lockee (analyze lockee)
     _ (set-val :position pos)]
    {::etype ::monitor-exit
     :lock lockee
     :java-type Void/TYPE
     :position pos}))

(defmethod gen ::monitor-enter
  [{:keys [lock position]}]
  `(~@(gen lock)
    [:monitorenter]
    [:aconst-null]
    ~@(maybe-pop position)))

(defmethod gen ::monitor-exit
  [{:keys [lock position]}]
  `(~@(gen lock)
    [:monitorexit]
    [:aconst-null]
    ~@(maybe-pop position)))

;;;; Assignment

(defmethod analyze [::special 'def]
  [[_ target init :as form]]
  (cond
    (> (count form) 3)
    (throw (Exception. "Too many arguments to def"))
    (< (count form) 2)
    (throw (Exception. "Too few arguments to def"))
    (not (symbol? target))
    (throw (Exception. "First argument to def must be a Symbol"))
    :else
    (let [^Var v (lookup-var *ns* target true)]
      (if (nil? v)
        (throw (Exception. "Can't refer to qualified var that doesn't exist"))
        (let [v (cond
                  (= (.ns v) *ns*) v
                  (nil? (namespace target)) (intern *ns* target)
                  :else (throw (Exception.
                                 "Can't create defs outside of current ns")))
              mm (meta target)]
          (when (:static mm)
            ;; Not the right place to do this...
            (alter-meta! v assoc
                         :static true
                         :arglists (second (:arglists mm))))
          (run [pos (update-val :position #(if (= % :eval) % :expression))
                meta-expr (analyze mm)
                n (set-val :name (name target))
                init-expr (analyze init)
                _ (set-val :name n)
;                v (analyze target) ;`(~'var ~target))
                _ (set-val :position pos)]
            {::etype ::def
             :metamap meta-expr
             :target v
             :java-type Var
             :init init-expr
             :position pos
             :init-provided? (== (count form) 3)}))))))

(defmethod gen ::def
  [{:keys [metamap target init init-provided? position]}]
  #_(println "Def target=" target "init:" init-provided?
           "meta=" metamap)
  `(~@(gen-constant target)
    ;[:checkcast ~Var]
    ;~@(when metamap
    ;    `([:dup]
    ;      ~@(gen metamap)
    ;      [:checkcast ~IPersistentMap]
    ;      [:invokevirtual ~[Var 'setMeta [:method :void [IPersistentMap]]]]))
    ~@(when init-provided?
        `([:dup]
          ~@(gen init)
          [:invokevirtual ~[Var 'bindRoot [:method :void [Object]]]]))
    ~@(maybe-pop position)))

(defmethod eval-toplevel ::def
  [{:keys [metamap target init init-provided?]} loader]
  (let [the-var target]
    (if init-provided?
      (.bindRoot the-var (eval-toplevel init loader))
      (if metamap
        (reset-meta! the-var (eval-toplevel metamap loader))))
    the-var))

(defmethod analyze [::special 'set!]
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

;; XXX just a quick hack
(defmethod analyze [::special 'case*]
  [[_ expr shift mask low high default vmap id? :as form]]
  (analyze (reduce (fn [part [k e]]
                     `(if (= ~expr '~k) ~e ~part))
                   default
                   (vals vmap))))

  #_(fn [ctx]
    (if (= (:position ctx) :eval)
      (analyze `((~'fn* [] ~form)))
      (run-with ctx
        [pos (set-val :position :expression)
         _ (set-val :want-unboxed nil)
         eexpr (analyze expr)
         _ (push-clear-node :branch false)
         evmap (m-map analyze-entry (seq vmap))
         _ pop-frame])))

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
      [:invokestatic ~[Symbol 'create [:method Symbol [String String]]]])

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
    (println "Unknown constant" c)))

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
        `([:ldc ~(int variadic-arity)]
          [:ireturn]))
      (asm/assemble-method mref)))
  (doseq [{:keys [argv bind this loop-label body] :as mm} (:methods obj)]
    (let [variadic? (variadic? argv)
          mref (asm/add-method cref
                   {:name (if variadic? 'doInvoke 'invoke)
                    :descriptor [:method Object
                                 (repeat (count bind) Object)]
                    :params (map :label bind)
                    :flags #{:public}
                    :throws [Exception]})]
      (asm/emit1 cref mref [:label loop-label])
      (asm/emit cref mref (gen body))
      (asm/emit1 cref mref [:areturn])
      (asm/assemble-method mref))))

(defn compile-class
  "Compile and load the class representing
  a fn or (not implemented yet) deftype"
  [loader obj super ifaces]
  (println "clazz" (:name obj))
  (asm/assembling [c {:name (:name obj)
                      :extends super
                      :implements ifaces
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
      (asm/assemble-method clinit))

    ;; instance fields for closed-overs
    (doseq [[sym bind] (:closed-lexicals obj)]
      (let [{:keys [gen-type]} bind]
        (asm/add-field c {:name sym
                          :descriptor gen-type
                          :flags #{:public :final}})))

    (let [clos (:closed-lexicals obj)
          clos-names (keys clos)
          clos-types (map :gen-type (vals clos))]
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
        (asm/assemble-method init)))

    (let [meta (asm/add-method c {:name 'meta
                                  :descriptor [:method IPersistentMap []]
                                  :flags #{:public}})]
      (asm/emit c meta
        `([:aconst-null]
          [:areturn]))
      (asm/assemble-method meta))

    (let [with-meta (asm/add-method c {:name 'withMeta
                                       :descriptor
                                         [:method IObj [IPersistentMap]]
                                       :flags #{:public}})]
      (asm/emit c with-meta
        `([:aload-0]
          [:areturn]))
      (asm/assemble-method with-meta))

    (emit-methods obj c)

    (let [c (asm/assemble-class c)
          bytecode (ByteBuffer/allocate (asm/class-length c))] 
      (bin/write-binary ::asm/ClassFile bytecode c)
      (let [arr (.array bytecode)]
        (try
          (.defineClass loader (str (:name obj)) arr nil)
          (catch Throwable e
            (println "Caught while loading class:" (class e) e))
          (finally
            (when *debug-inspect*
              (.clear bytecode)
              (inspect-tree (bin/read-binary ::asm/ClassFile bytecode)))))))
    loader))

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
   (asm/assembling [ns-init {:name (file->class-name src-path)
                             :flags #{:public :super}
                             :source src-name}]
     (let [loader (DynamicClassLoader.)]
       (run-with (assoc null-context :loader loader)
         [_ (push-object-frame {:name 'toplevel})
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
                 (asm/assemble-method clj-init))

               (let [[top _] (current-object context)]
                 (doseq [fld (:constants top)]
                   (let [{:keys [val gen-type]} fld]
                     (asm/add-field ns-init {:name val
                                             :descriptor gen-type
                                             :flags #{:public :final :static}})))
           ;; TODO inits in blocks of 100...
                 (let [init-const (asm/add-method ns-init
                                                  {:name '__init0
                                                   :descriptor [:method :void []]
                                                   :flags #{:public :static}})]
                   (emit-constants top ns-init init-const)
                   (asm/emit1 ns-init init-const [:return])
                   (asm/assemble-method init-const))

                 (let [clinit (asm/add-method ns-init
                                        {:name '<clinit>
                                         :descriptor [:method :void []]
                                         :flags #{:public :static}})
                       [start-try end-try end final]
                       (map gensym ["TRY__" "DONE__" "END__" "FIN__"])]
                   (asm/emit ns-init clinit
                     `([:iconst-2] ; inlined Compiler/pushNS()
                       [:anewarray [:array ~Object]]
                       [:dup]
                       [:iconst-0] ; arr arr 0
                       [:ldc "clojure.core"]
                       [:invokestatic ~[Symbol 'create
                                        [:method Symbol [String]]]]
                       [:ldc "*ns*"]
                       [:invokestatic ~[Symbol 'create
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
                   (asm/assemble-method clinit))
                 (let [ns-init (asm/assemble-class ns-init)]))))))))))

(defn- compile1
  "Compile and eval a top-level form"
  [form]
  (run [form (macroexpand-impl *ns* form)
        res (if (and (seq? form) (= (first form) 'do))
              (m-map compile1 (next form))
              (run [analyzed (analyze form)
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

(defn compile
  "Compile and load a lib and write generated class files to *compile-path*"
  [lib]
  (let [root (root-resource lib)
        cljfile (.substring (str root ".clj") 1)
        loader (RT/baseLoader)]
    (with-open [ins (.getResourceAsStream loader cljfile)
                inrd (InputStreamReader. ins (Charset/forName "UTF-8"))]
      (binding [*ns* *ns*
                *compile-files* true]
        (compile-file inrd cljfile
          (.substring cljfile (inc (.lastIndexOf cljfile "/")))))))
  nil)
