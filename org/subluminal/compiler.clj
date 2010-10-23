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

(defn push-object-frame [f]
  (let [tag (gensym "OBJ__")
        f (assoc f
                 :fn-tag tag
                 :constants []
                 :constant-ids (IdentityHashMap.)
                 :keywords {}
                 :vars {}
                 :keyword-callsites {}
                 :var-callsites {}
                 :closed-lexicals (sorted-map))]
    (fn [ctx]
      (let [enc (:method ctx)]
        [nil (assoc ctx
                    :parent ctx
                    :fn tag
                    :index (assoc (:index ctx) tag
                                  (assoc f :enclosing-method enc)))]))))

(defn push-method-frame [m]
  (let [tag (gensym "MT__")
        ct (gensym "CN__")
        m (assoc m :method-tag tag)]
    (fn [ctx]
      (let [obj (:fn ctx)]
        [nil
         (assoc ctx
                :position :return
                :parent ctx
                :method tag
                :loop-label (:loop-label m)
                :catching nil
                :clear-tag ct
                :clear-path ctx
                :clear-root ct
                :clear-kind :path
                :index (assoc (:index ctx) tag
                              (assoc m :containing-object obj)))]))))

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

(defn make-binding
  ([sym jtype] (make-binding sym jtype :local nil))
  ([sym jtype kind lbl]
   (let [jtype (if jtype jtype Object)]
     (fn [ctx]
       (let [b {:symbol sym
                :label (or lbl (gensym "BB"))
                :force-live (when lbl true) ; don't clear 'this
                :kind kind
                :fn-tag (:fn ctx)
                :java-type jtype
                :clear-root (:clear-root ctx)}]
         [b (update-in ctx [:lexicals]
                       assoc sym b)])))))

;; the assembler recognizes 'this as a special label
(defn make-this-binding
  [sym jtype]
  (make-binding sym jtype :fnarg 'this))

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

(defn- ensure-primitive
  [^Class cls]
  (when (and cls (.isPrimitive cls) cls)))

(defn gen-conversion [ttyp styp]
  (if (not= ttyp styp) (println "Request conv" ttyp "<-" styp))
  (if (and styp (.isPrimitive styp))
    (cond
      (= ttyp styp)
      nil
      (is-primitive? ttyp)
      `(~@(gen-boxed styp)
        ~@(coerce-primitive ttyp))
      :else
      `(~@(gen-boxed styp)
        ~@(if (and ttyp (not (isa? styp ttyp)))
            [[:checkcast ttyp]])))
    (cond
      (= ttyp styp)
      nil
      (is-primitive? ttyp)
      (coerce-primitive ttyp)
      :else
      (if (and ttyp (not (isa? styp ttyp)))
        [[:checkcast ttyp]]))))

(defn gen-typed-arg [typ arg]
  (let [atyp (:java-type arg)
        acode (gen arg)]
    `(~@acode
      ~@(gen-conversion typ atyp))))

(defn- coerce-primitive
  "Generate bytecode to coerce the object on stack to a primitive type"
  [prim]
  (condp = prim
    Boolean/TYPE
    `([:checkcast ~Boolean]
      [:invokevirtual ~[Boolean 'booleanValue [:method :boolean []]]])
    Character/TYPE
    `([:checkcast ~Character]
      [:invokevirtual ~[Character 'charValue [:method :char []]]])
    Byte/TYPE
    `([:checkcast ~Number]
      [:invokevirtual ~[Number 'byteValue [:method :byte []]]])
    Short/TYPE
    `([:checkcast ~Number]
      [:invokevirtual ~[Number 'shortValue [:method :short []]]])
    Long/TYPE
    `([:checkcast ~Number]
      [:invokevirtual ~[Number 'longValue [:method :long []]]])
    Integer/TYPE
    `([:checkcast ~Number]
      [:invokevirtual ~[Number 'intValue [:method :int []]]])
    Float/TYPE
    `([:checkcast ~Number]
      [:invokevirtual ~[Number 'floatValue [:method :float []]]])
    Double/TYPE
    `([:checkcast ~Number]
      [:invokevirtual ~[Number 'doubleValue [:method :double []]]])))

(defn gen-boxed [orig-type]
  (cond
    (= orig-type Void/TYPE)
    `([:aconst-null])
    (= orig-type Boolean/TYPE)
    `([:invokestatic ~[Boolean 'valueOf [:method Boolean [:boolean]]]])
    (= orig-type Character/TYPE)
    `([:invokestatic ~[Character 'valueOf [:method Character [:char]]]])
    (= orig-type Byte/TYPE)
    `([:invokestatic ~[Byte 'valueOf [:method Byte [:byte]]]])
    (= orig-type Short/TYPE)
    `([:invokestatic ~[Short 'valueOf [:method Short [:short]]]])
    (= orig-type Integer/TYPE)
    `([:invokestatic ~[Integer 'valueOf [:method Integer [:int]]]])
    (= orig-type Long/TYPE)
    `([:invokestatic ~[Long 'valueOf [:method Long [:long]]]])
    (= orig-type Float/TYPE)
    `([:invokestatic ~[Float 'valueOf [:method Float [:float]]]])
    (= orig-type Double/TYPE)
    `([:invokestatic ~[Double 'valueOf [:method Double [:double]]]])))

(defn tea "Test analysis"
  ([form] (tea form :expression))
  ([form pos] (tea form pos (DynamicClassLoader.)))
  ([form pos loader]
   (let [[res ctx]
         (run-with (assoc null-context :loader loader)
           [_ (push-object-frame {:name 'toplevel})
            _ (set-val :position pos)
            a (analyze form)
            _ pop-frame]
           a)]
     res)))

(defn teg "Test gen"
  ([form] (gen (tea form)))
  ([form pos] (gen (tea form pos))))

(defn tee 
  "Test eval-toplevel"
  [form]
  (let [ldr (DynamicClassLoader.)]
    (eval-toplevel (tea form :eval ldr) ldr)))

;;;; Nonempty collections

(defmethod analyze ::vector
  [vect]
  (run [pos (set-val :position :expression)
        comps (m-map analyze vect)
        _ (set-val :position pos)
        res (if (every? #(isa? (::etype %) ::constant) comps)
              (register-constant (vec (map :orig comps)))
              (m-result {::etype ::vector
                         :components comps
                         :java-class IPersistentVector
                         :position pos}))]
    ;; TODO metadata
    res))

(defmethod analyze ::set
  [elts]
  (run [pos (set-val :position :expression)
        elts (m-map analyze elts)
        _ (set-val :position pos)
        res (if (every? #(isa? (::etype %) ::constant) elts)
              (register-constant (set (map :orig elts)))
              (m-result {::etype ::set
                         :elements elts
                         :java-class IPersistentSet
                         :position pos}))]
       res))

(defmethod analyze ::map
  [entries]
  (run [pos (set-val :position :expression)
        entries (m-map analyze (apply concat (seq entries)))
        _ (set-val :position pos)
        res (if (every? #(isa? (::etype %) ::constant) entries)
              (register-constant (apply array-map (map :orig entries)))
              (m-result {::etype ::map
                         :entries entries
                         :java-class IPersistentMap
                         :position pos}))]
       res))

(defmethod gen ::vector
  [{:keys [components position]}]
  `(~@(gen-array components)
    [:invokestatic
     ~[RT 'vector [:method IPersistentVector [[:array Object]]]]]
    ~@(maybe-pop position)))

(defmethod gen ::set
  [{:keys [elements position]}]
  `(~@(gen-array elements)
    [:invokestatic
     ~[RT 'set [:method IPersistentSet [[:array Object]]]]]
    ~@(maybe-pop position)))

(defmethod gen ::map
  [{:keys [entries position]}]
  `(~@(gen-array entries)
    [:invokestatic
     ~[RT 'map [:method IPersistentMap [[:array Object]]]]]
    ~@(maybe-pop position)))

(defmethod eval-toplevel ::vector
  [{:keys [components]} loader]
  (into [] (map #(eval-toplevel % loader) components)))

(defmethod eval-toplevel ::set
  [{:keys [elements]} loader]
  (into #{} (map #(eval-toplevel % loader) elements)))

(defmethod eval-toplevel ::map
  [{:keys [entries]} loader]
  (apply hash-map (map #(eval-toplevel % loader) entries)))

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

;;;; Host expressions

(defmethod analyze [::special 'new]
  [[_ cls & ctor-args :as form]]
  (if (< (count form) 2)
    (throw (Exception. (str "Wrong number of arguments, "
                            "expecting (new Classname args...)")))
    (let [^Class c (maybe-class *ns* cls false)]
      (if-not c
        (throw (IllegalArgumentException.
                 (str "Unable to resolve classname: " cls)))
        (run [pos (set-val :position :expression)
              _ (set-val :want-unboxed true)
              args (m-map analyze ctor-args)
              _ (set-val :position pos)]
          (let [ctors (filter (fn [^Constructor ctor]
                                (= (count (.getParameterTypes ctor))
                                   (count args)))
                              (.getConstructors c))
                match
                (cond
                  (empty? ctors)
                  (throw (Exception. (str "No matching ctor found for " c)))
                  (not (next ctors))
                  (first ctors)
                  :else
                  (matching-constructor (map :java-type args) ctors))]
            {::etype ::new
             :java-type c
             :ctor match
             :args args
             :position pos}))))))

(defmethod gen ::new
  [{:keys [^Class java-type ^Constructor ctor args position]}]
  (if ctor
    `([:new ~java-type]
      [:dup]
      ~@(mapcat gen-typed-arg (seq (.getParameterTypes ctor)) args)
      [:invokespecial ~ctor]
      ~@(maybe-pop position))
    `([:ldc ~(.getName java-type)]
      [:invokestatic ~[Class 'forName [:method Class [String]]]]
      ~@(gen-array args)
      [:invokestatic ~[Reflector 'invokeConstructor
                       [:method Object [Class [:array Object]]]]]
      ~@(maybe-pop position))))

(defmethod eval-toplevel ::new
  [{:keys [java-type ^Constructor ctor args]} loader]
  (let [argvals (into-array Object (map #(eval-toplevel % loader) args))]
    (if ctor
      (.newInstance ctor
                    (box-args (.getParameterTypes ctor) argvals))
      (Reflector/invokeConstructor java-type argvals))))

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

;;;; Closure

(defn normalize-fn*
  [[op & opts :as form]]
  (let [this-name (when (symbol? (first opts)) (first opts))
        static? (:static (meta this-name))]
    (run
      [n (if this-name
           (m-result this-name)
           (fetch-val :name))
       enc (fetch-val :enclosing-method)]
    (let [opts (if (symbol? (first opts))
                 (next opts)
                 opts)
          methods (if (vector? (first opts))
                    (list opts)
                    opts)
          base-name (if enc
                      (str (:name enc) "$")
                      (str (munge-impl (name (ns-name *ns*))) "$"))
          simple-name (if n
                        (str (.replace (munge-impl (name n)) "." "_DOT_")
                             (if enc (str "__" (RT/nextID)) ""))
                        (str "fn__" (RT/nextID)))]
      (with-meta `(~'fn* ~@methods)
                 {:src form
                  :this-name n ; symbol used for self-recursion
                  :enclosing-method enc
                  :static? static?
                  :once-only (:once (meta op))
                  :fn-tag (gensym "FN")
                  ;; name of generated class
                  :name (symbol (str base-name simple-name))})))))

(declare analyze-method compile-class)
(defn variadic? [argv]
  (not (nil? (:rest-param argv))))

(def +max-positional-arity+ 20)
(def +variadic-index+ (inc +max-positional-arity+))

(defn arity [m]
  (count (:required-params (:argv m))))

(defn- update-method-array [arr [info body :as m]]
  (let [arty (arity info)]
    (if (variadic? (:argv info))
      (if (arr +variadic-index+)
        (throw (Exception. "Can't have more than 1 variadic overload"))
        (assoc arr +variadic-index+ m))
      (if (arr arty)
        (throw (Exception. "Can't have 2 overloads with same arity"))
        (assoc arr arty m)))))

(defmethod analyze [::special 'fn*]
  [[op & opts :as form]]
  (run [classloader (fetch-val :loader)
        pos (fetch-val :position)
        [op & meth :as norm] (normalize-fn* form)
        _ (push-object-frame (meta norm))
        _ (set-val :name nil)
        meth (m-reduce update-method-array
                       (vec (repeat (+ 2 +max-positional-arity+) nil))
                       (map analyze-method meth))
        f current-object
        _ pop-frame
        _ (set-val :want-unboxed true)
        initargs (m-map analyze (map :symbol (vals (:closed-lexicals f))))]
    (let [[variadic-info _] (meth +variadic-index+)
          variadic-arity (when variadic-info (arity variadic-info))]
      (when (and variadic-info
                 (some identity (subvec meth (inc variadic-arity)
                                        +variadic-index+)))
        (throw (Exception.
                 (str "Can't have fixed arity function"
                      " with more params than variadic function"))))
      (when (and (:static? f)
                 (not (empty? (:closed-lexicals f))))
        (throw (Exception. "Static fns can't be closures")))
      ;; TODO: add metadata from original form
      (let [obj (assoc f :methods (filter identity meth)
                         :form form
                         :variadic-arity variadic-arity)]
        (compile-class classloader obj (if variadic-info RestFn AFunction) [])
        {::etype ::fn
         :class-name (:name obj)
         :initargs initargs
         :position pos}))))

(defmethod gen ::fn
  [{:keys [class-name initargs position]}]
  `([:new ~class-name]
    [:dup]
    ~@(apply concat
        (for [b initargs]
          (gen b)))
    [:invokespecial ~[class-name '<init>
                      [:method :void (for [init initargs]
                                       (or (:unbox-type init)
                                           (:java-type init)))]]]
    ~@(maybe-pop position)))

(defmethod eval-toplevel ::fn
  [{:keys [class-name initargs]} loader]
  (let [^Class c (.loadClass loader (str class-name))
        eargs (doall (map #(eval-toplevel % loader) initargs))
        ^Constructor ctor
        (.getConstructor c (into-array Class (map :java-type initargs)))]
    (.newInstance ctor (into-array Object eargs))))

(defn process-fn*-args [argv static?]
  (loop [req-params [] rest-param nil state :req remain argv]
    (if-not (seq remain)
      {:required-params req-params
       :rest-param rest-param}
      (let [arg (first remain)]
        (cond
          (not (symbol? arg))
          (throw (Exception. "fn params must be symbols."))

          (namespace arg)
          (throw (Exception. "Can't use qualified name as parameter: " arg))

          (= arg '&)
          (if (= state :req)
            (recur req-params rest-param :rest (next remain))
            (throw (Exception. "Invalid parameter list")))

          :else
          (let [c (tag-class *ns* (tag-of arg))
                c (if (= state :rest) ISeq c)]
            (cond
              (and (.isPrimitive c) (not static?))
              (throw (Exception.
                (str "Non-static fn can't have primitive parameter: " arg)))

              (and (.isPrimitive c)
                   (not (or (= c Long/TYPE) (= c Double/TYPE))))
              (throw (IllegalArgumentException.
                (str "Only long and double primitives are supported: " arg)))

              (and (= state :rest) (not= (tag-of arg) nil))
              (throw (Exception. "& arg can't have type hint")))
            (recur (if (= state :req)
                     (conj req-params [arg c])
                     req-params)
                   (if (= state :rest)
                     [arg c]
                     nil)
                   (if (= state :rest)
                     :done
                     state)
                   (next remain))))))))

(defn analyze-method
  [[argv & body]]
  (if (not (vector? argv))
    (throw (IllegalArgumentException.
             "Malformed method, expected argument vector"))
    (let [ret-class (tag-class *ns* (tag-of argv))]
      (when (and (.isPrimitive ret-class)
                 (not (or (= ret-class Long/TYPE) (= ret-class Double/TYPE))))
        (throw (IllegalArgumentException.
                 "Only long and double primitives are supported")))
      (run [{:keys [this-name static?] :as this-fn} current-object
            argv (m-result (process-fn*-args argv static?))
            _ (push-method-frame {:loop-label (gensym "LL")
                                  :loop-locals nil})
            thisb (if static?
                    (m-result nil)
                    (make-this-binding
                      (or this-name (gensym "THIS")) IFn))
            bind (m-map (fn [[s t]] (make-binding s t :fnarg nil))
                        (if (variadic? argv)
                          (conj (:required-params argv)
                                (:rest-param argv))
                          (:required-params argv)))
            _ (update-current-method assoc :loop-locals bind)
            _ (set-val :loop-locals bind)
            _ (set-val :want-unboxed (when (.isPrimitive ret-class)
                                       ret-class))
            body (analyze `(~'do ~@body))
            m current-method
            _ pop-frame]
        [(assoc m
                :argv argv
                :bind bind
                :this thisb)
                body]))))

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
  (doseq [{:keys [val java-type obj orig]} (:constants obj)]
;    (println "Const type=" java-type "orig=" orig)
    (asm/emit cref mref
      `(~@(gen-constant orig)
        ~@(if java-type [[:checkcast java-type]])
        [:putstatic [~obj ~val ~(or java-type Object)]]))))

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
  (doseq [[info body :as mm] (:methods obj)]
    (let [{:keys [argv bind this loop-label]} info
          variadic? (variadic? argv)]
      (let [mref (asm/add-method cref
                   {:name (if variadic? 'doInvoke 'invoke)
                    :descriptor [:method Object
                                 (repeat (count bind) Object)]
                    :params (map :label bind)
                    :flags #{:public}
                    :throws [Exception]})]
        (asm/emit1 cref mref [:label loop-label])
        (asm/emit cref mref (gen body))
        (asm/emit1 cref mref [:areturn])
        (asm/assemble-method mref)))))

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
      (let [{:keys [val java-type]} fld]
        (asm/add-field c {:name val
                          :descriptor (or java-type Object)
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
      (let [{:keys [java-type]} bind]
        (asm/add-field c {:name sym
                          :descriptor java-type
                          :flags #{:public :final}})))

    (let [clos (:closed-lexicals obj)
          clos-names (keys clos)
          clos-types (map :java-type (vals clos))]
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
                   (let [{:keys [val java-type]} fld]
                     (asm/add-field ns-init {:name val
                                             :descriptor java-type
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
