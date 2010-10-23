(in-ns 'org.subluminal.compiler)

;;;; Atomic expressions

(defn tea "Test analysis"
  ([form] (tea form :expression nil))
  ([form pos typ] (tea form pos typ (DynamicClassLoader.)))
  ([form pos typ loader]
   (let [[res ctx]
         (run-with (assoc null-context :loader loader)
           [_ (push-object-frame {:name 'toplevel})
            a (analyze pos typ nil form)
            _ pop-frame]
           a)]
     res)))

(defn teg "Test gen"
  ([form] (gen (tea form)))
  ([form pos] (gen (tea form pos nil)))
  ([form pos typ] (gen (tea form pos typ))))

(defn tee 
  "Test eval-toplevel"
  [form]
  (let [ldr (DynamicClassLoader.)]
    (eval-toplevel (tea form :eval nil ldr) ldr)))

;;;; Literal objects embedded in the source form
;;;; that don't fall into one of the more specific types
;;;; Cannot be of primitive type

(defn register-constant
  [pos obj]
  (fn [ctx]
    (let [curr-obj (get (:index ctx) (:fn ctx))
          gen-type (if (= pos :statement) Void/TYPE (class obj))]
      (if-let [c (get (:constant-ids curr-obj) obj)]
        [(assoc c :gen-type gen-type) ctx]
        (let [c {::etype ::constant
                 :gen-type (class obj)
                 :cfield (gensym "const__")
                 :lit-val obj
                 :obj (:name curr-obj)}
              curr-obj (update-in curr-obj [:constants] conj c)]
          (.put (:constant-ids curr-obj) obj c)
          [(assoc c :gen-type gen-type)
           (assoc-in ctx [:index (:fn ctx)] curr-obj)])))))

(defmethod analyze ::constant
  [pos typ _ obj]
  (register-constant pos obj))

(defmethod gen ::constant
  [{:keys [gen-type lit-val obj cfield]}]
  {:pre [(or (= gen-type Void/TYPE) (= gen-type (class lit-val)))]}
  (if (= gen-type Void/TYPE)
    ()
    `([:getstatic ~[obj cfield (class lit-val)]])))

(defmethod eval-toplevel ::constant
  [{:keys [lit-val]} loader]
  lit-val)

;;;; nil literal

(defmethod analyze ::null
  [pos _ _ form]
  (run []
    {::etype ::null
     :gen-type (if (= pos :statement) Void/TYPE nil)}))

(defmethod gen ::null
  [{:keys [gen-type]}]
  (gen-convert Void/TYPE gen-type))

(defmethod eval-toplevel ::null
  [_ _]
  nil)

;;;; true, false literals

(derive ::boolean ::constant)

(defmethod analyze ::boolean
  [pos typ _ form]
  (run []
    {::etype ::boolean
     :lit-val form
     :source-type Boolean/TYPE
     :gen-type
     (cond
       (= pos :statement)
         Void/TYPE
       (or (true? typ)
           (= typ Boolean/TYPE))
         Boolean/TYPE
       :else
         Boolean)}))

(defmethod gen ::boolean
  [{:keys [lit-val source-type gen-type]}]
  {:pre [(gen-contract source-type gen-type)]}
  (cond
    (= gen-type Void/TYPE) ()
    (= gen-type Boolean/TYPE)
    (if lit-val
      `([:iconst-1])
      `([:iconst-0]))
    :else
    (if val
      `([:getstatic ~[Boolean 'TRUE Boolean]])
      `([:getstatic ~[Boolean 'FALSE Boolean]]))))

;; eval-toplevel --> ::constant

;;;; Number literals
;; Only long and double primitives are allowed
;; char literals are always analyzed as boxed Character ::constant

(derive ::number ::constant)

(defn promote-literal [x]
  (cond
    (or (instance? Byte x)
        (instance? Short x)
        (instance? Integer x))
    (long x)
    (instance? Float x)
    (double x)
    :else x))

(defmethod analyze ::number
  [pos typ _ form]
  (let [nval (promote-literal form)]
    (if (or (= pos :statement)
            (and (instance? Long nval)
                 (or (true? typ) (= typ Long/TYPE)))
            (and (instance? Double nval)
                 (or (true? typ) (= typ Double/TYPE))))
      (run []
        (let [styp (if (instance? Long nval) Long/TYPE Double/TYPE)]
          {::etype ::number
           :lit-val nval
           :source-type styp
           :gen-type (if (= pos :statement) Void/TYPE styp)}))
      (register-constant pos nval))))

(def
  ^{:private true
    :doc "Instructions to generate small integer constants"}
  tiny-ints
  {-1 [:iconst-m1]
   0 [:iconst-0]
   1 [:iconst-1]
   2 [:iconst-2]
   3 [:iconst-3]
   4 [:iconst-4]
   5 [:iconst-5]})

(defmethod gen ::number
  [{:keys [lit-val source-type gen-type]}]
  (cond
    (= gen-type Void/TYPE) ()
    (and (= gen-type Long/TYPE)
         (== lit-val 0))
    `([:lconst-0])
    (and (= gen-type Long/TYPE)
         (== lit-val 1))
    `([:lconst-1])
    (and (= gen-type Long/TYPE)
         (<= -0x80 lit-val 0x7F))
    `(~(tiny-ints lit-val [:bipush lit-val]) [:i2l])
    (and (= gen-type Long/TYPE)
         (<= -0x8000 lit-val 0x7FFF))
    `([:sipush ~lit-val] [:i2l])
    (and (= gen-type Double/TYPE)
         (== lit-val 0.0))
    `([:dconst-0])
    (and (= gen-type Double/TYPE)
         (== lit-val 1.0))
    `([:dconst-1])
    :else
    `([:ldc2-w ~lit-val])))

;; eval-toplevel --> ::constant

;;;; String literals

(defmethod analyze ::string
  [pos typ _ ^String form]
  (register-constant pos (.intern form)))

;; gen, eval-toplevel --> ::constant

;;;; Symbols
;; Analyze as local binding, Var, class constant, or static field

;; liveness analysis for locals clearing

(defn- clear-path [b]
  (let [p (reverse (next (take-while identity (iterate :clear-path b))))]
    p))

(defn- join-point [b1 b2]
  (loop [p1 (clear-path b1) p2 (clear-path b2)]
    (cond
      (not= (:clear-tag (first p1)) (:clear-tag (first p2)))
      nil

      (or (nil? (second p1)) (not= (:clear-tag (second p1))
                                   (:clear-tag(second p2))))
      (first p1)

      :else
      (recur (next p1) (next p2)))))

;; One bit of mutable state. *sigh*
;; It would be possible to get rid of this by walking
;; the expressions in opposite order for liveness analysis
(defn- make-binding-instance [b tag]
  (let [gen (or tag (:gen-type b))
        gen (if (gen-contract (:source-type b) gen)
              gen
              ;; warn about bad type hint
              (:gen-type b))]
    (fn [ctx]
      (let [inst (assoc b :clear-path ctx
                          :gen-type gen
                          ::etype ::local-binding
                          :live (atom (or (:force-live b)
                                          (not= (:clear-root b)
                                                (:clear-root ctx)))))

            ;; Mark previous references in this branch live
            lives (get-in ctx [:binding-sites (:label b)])
            lives (reduce (fn [coll inst2]
                            (let [j (join-point inst inst2)]
                              (if (not= (:clear-kind j) :branch)
                                (do (reset! (:live inst2) true) coll)
                                (conj coll inst2))))
                          []
                          lives)]
        [(dissoc inst :clear-path)
         (assoc-in ctx [:binding-sites (:label b)]
                   (conj lives inst))]))))

(defn- close-over
  "Make the current method a closure over b"
  [b]
  (fn [ctx]
    (let [curr-obj (first (current-object ctx))]
      (loop [obj (:fn ctx) ctx ctx]
        (assert obj) ; can't have locals in :eval position
        (if (= obj (:fn-tag b))
          (if (= obj (:fn-tag curr-obj))
            [b ctx] ; already a local in this method
            [(assoc b :kind :closed
                      :place (:name curr-obj))
             ctx])
          (recur (->> obj
                   (get (:index ctx))
                   :enclosing-method
                   (get (:index ctx))
                   :containing-object)
                 ;; intervening fns will also close over b
                 (update-in ctx [:index obj :closed-lexicals]
                            assoc (:label b)
                            (assoc b :kind :closed
                                     :place (:name (get :index ctx) obj)))))))))

(defn register-var [pos v]
  (fn [ctx]
    (let [curr-obj (get (:index ctx) (:fn ctx))]
      (if-let [prev (get (:vars curr-obj) v)]
        [(assoc prev :gen-type (if (= pos :statement)
                                 Void/TYPE
                                 (:gen-type prev)))
         ctx]
        (let [[c ctx] ((register-constant pos v) ctx)
              vv (assoc c ::etype ::var
                          :gen-type Object)]
          [vv (update-in ctx [:index (:fn ctx) :vars]
                         assoc v vv)])))))

(defn resolve-lexical
  "Return the visible lexical binding of sym if any.
  The current method will be made closure over the binding"
  [sym]
  (run [b (fetch-in :lexicals sym)
        r (if b
            (close-over b)
            (m-result nil))]
    r))

;; Try to resolve (in order):
;; 1. fully.qualified.Class or [LArrayClass;
;;    -> Class object or ClassNotFoundException
;; 2. always resolve the symbols 'ns and 'in-ns in ns clojure.core
;; (2b. compiler-stub)
;; 3. ns-map of rel-ns
;;    -> interned Var, referred Var
;;       or imported Class
;; (3b. *allow-unresolved-vars*)
;; 4. throw "unable to resolve"
;;       
;; Not using ns-resolve since that fn delegates to clojure.lang.Compiler
(defn resolve-unqualified [^Namespace rel-ns sym]
  (cond
    (or (>= (.indexOf (name sym) (int \.)) 0) (= (.charAt (name sym) 0) \[))
    (RT/classForName (name sym))
    
    (= sym 'ns) #'clojure.core/ns
    (= sym 'in-ns) #'clojure.core/in-ns

    :else
    (if-let [o (.getMapping rel-ns sym)]
      o
      (throw (Exception. (str "Unable to resolve " sym
                              " in this context"))))))

;; Try to resolve (in order):
;; 1. ns-alias/interned-var
;;    or qualified.ns/interned-var
;;    -> Var or throw if not found and public
;; 2. fully.qualified.Class/staticField
;;    or ImportedClass/staticField
;;    -> java.reflect.Field or throw if no such field
;; 3. throw "no such namespace"
(defn resolve-qualified [^Namespace rel-ns sym]
  (let [ns-part (symbol (namespace sym))
        ns-for (or (.lookupAlias rel-ns ns-part)
                   (Namespace/find ns-part))]
    (if ns-for
      (let [v (.findInternedVar ns-for (symbol (name sym)))]
        (cond
          (nil? v)
          (throw (Exception. (str "No such var: " sym)))
          (and (not= ns-for rel-ns) (not (.isPublic v)))
          (throw (IllegalStateException.
                   (str "var: " sym " is not public")))
          :else v))
      (if-let [^Class c (maybe-class rel-ns ns-part false)]
        (if-let [f (Reflector/getField c (name sym) true)]
          ;; TODO: should be same as (. Class staticField)
          {::etype ::static-field
           :target c
           :member f
           :source-type (.getType f)
           :gen-type (.getType f)}
          (throw (Exception. (str "Unable to find static field: " (name sym)
                                  " in " c))))
        (throw (Exception. (str "No such namespace: " ns-part)))))))

(defn resolve-sym [n sym]
  (if (namespace sym)
    (resolve-qualified n sym)
    (resolve-unqualified n sym)))

(defn lookup-sym [pos n sym]
  (let [o (resolve-sym n sym)]
    (cond
      (var? o)
      (if (.isMacro o)
        (throw (Exception. (str "Can't take value of a macro: " o)))
        (register-var pos o))
      (class? o)
      (register-constant pos o)
      :else
      (with-monad state-m (m-result o)))))

(defmethod analyze ::symbol
  [pos typ _ ^Symbol sym]
  (let [hint (tag-class *ns* (tag-of sym))]
    (run [stat (fetch-state)
          lex (resolve-lexical sym)
          bind (if lex
                 (make-binding-instance lex hint)
                 (lookup-sym pos *ns* sym))]
      (condp = (::etype bind)
        ::local-binding
        (let [gen-type (:gen-type bind)
              gen-type (cond
                         (= pos :statement) Void/TYPE
                         (not (analyze-contract typ gen-type))
                         (do
                           ;; warn about inappropriate primitive type hint
                           (boxed-version gen-type gen-type))
                         :else gen-type)]
          (assoc bind :gen-type gen-type))

        ::var
        (let [gen (or hint (:gen-type bind))
              gen (cond
                    (= pos :statement) Void/TYPE
                    (not (analyze-contract typ gen))
                    (boxed-version gen gen)
                    :else gen)]
          (assoc bind :gen-type gen))

        ::constant bind ; class constant
        ::static-field
        ; warn about type hint?
        (let [gen (:gen-type bind)
              gen (cond
                    (= pos :statement)
                    Void/TYPE
                    (not (analyze-contract typ gen))
                    (boxed-version gen gen)
                    :else gen)]
          (assoc bind :gen-type gen))))))

(derive ::var ::constant)

(defmethod eval-toplevel ::var
  [{:keys [lit-val]} loader]
  (deref lit-val))

(defmethod gen ::var
  [{:keys [gen-type cfield obj]}]
  `([:getstatic ~[obj cfield Var]]
    [:invokevirtual ~[Var 'get [:method Object []]]]
    ~@(gen-coerce Object gen-type)))

;; using :iconst-0 :istore
;; instead of :aconst-null :astore
;; This way the bytecode verifier will catch too aggressive locals clearing
(defn- maybe-clear-local
  "Generate bytecode to clear a local binding after use"
  [{:keys [kind gen-type source-type label live place]}]
  {:pre [(gen-contract source-type gen-type)]}
  (cond
    (is-primitive? source-type) ()
    (= kind :closed) nil ; TODO: :once fns
    @live nil
    :else (list [:iconst-0] [:istore label])))

(defmethod gen ::local-binding
  [{:keys [kind gen-type source-type place label] :as lb}]
  `(~@(if (= kind :closed)
        (list [:aload-0] [:getfield [place label source-type]])
        (list [(load-op source-type) label]))
    ~@(maybe-clear-local lb)
    ~@(gen-convert source-type gen-type)))

;;;; Var special form

(derive ::the-var ::constant)

(defmethod analyze [::special 'var]
  [pos typ _ [_ vname :as form]]
  (let [v (lookup-var *ns* vname false)]
    (if v
      (run [cv (register-var pos v)]
        (assoc cv ::etype ::the-var))
      (throw (Exception. (str "Unable to resolve var: " vname
                              " in this context"))))))

;;;; Keyword literal

(derive ::keyword ::constant)

(defn register-keyword [pos kw]
  (fn [ctx]
    (let [curr-obj (get (:index ctx) (:fn ctx))]
      (if-let [prev (get (:keywords curr-obj) kw)]
        [(assoc prev :gen-type
                     (if (= pos :statement)
                       Void/TYPE
                       (:gen-type prev)))
         ctx]
        (let [[c ctx] ((register-constant pos kw) ctx)
              k (assoc c ::etype ::keyword)]
          [k (update-in ctx [:index (:fn ctx) :keywords]
                        assoc kw k)])))))

(defmethod analyze ::keyword
  [pos typ _ kw]
  (register-keyword pos kw))

;;;; Quote special form

(defmethod analyze [::special 'quote]
  [pos typ _ [_ obj]]
  (register-constant pos obj))

;;;; Empty collections

(derive ::empty ::constant)

(defmethod analyze ::empty
  [pos typ _ coll]
  ;; TODO metadata
  (run []
    {::etype ::empty
     :gen-type
     (cond
       (= pos :statement) Void/TYPE
       (vector? coll) IPersistentVector
       (set? coll) IPersistentSet
       (map? coll) IPersistentMap
       (seq? coll) ISeq)
     :lit-val (if (instance? LazySeq coll) () coll)}))

(defmethod gen ::empty
  [{:keys [lit-val gen-type]}]
  (cond
    (= gen-type Void/TYPE) ()
    (vector? lit-val)
    `([:getstatic ~[PersistentVector 'EMPTY PersistentVector]])
    (set? lit-val)
    `([:getstatic ~[PersistentHashSet 'EMPTY PersistentHashSet]])
    (map? lit-val)
    `([:getstatic ~[PersistentArrayMap 'EMPTY PersistentArrayMap]])
    (seq? lit-val)
    `([:getstatic ~[PersistentList 'EMPTY PersistentList$EmptyList]])
    :else (throw (UnsupportedOperationException.
                   (str "Unknown collection type " (class lit-val))))))

;;;; Import

(defmethod analyze [::special 'clojure.core/import*]
  [pos typ _ [_ cname :as form]]
  (if (not (string? cname))
    (throw (Exception. (str "Expected (import* class-name), got: " cname)))
    (run []
      {::etype ::import
       :gen-type (if (= pos :statement) Void/TYPE Class)
       :target cname})))

(defmethod gen ::import
  [{:keys [target gen-type]}]
  `([:getstatic ~[RT 'CURRENT_NS Var]]
    [:invokevirtual ~[Var 'deref [:method Object []]]]
    [:checkcast ~Namespace]
    [:ldc ~target]
    [:invokestatic ~[Class 'forName [:method Class [String]]]]
    [:invokevirtual ~[Namespace 'importClass [:method Class [Class]]]]
    ~@(gen-convert Class gen-type)))

(defmethod eval-toplevel ::import
  [{:keys [target]} loader]
  (.importClass *ns* (Class/forName target)))
