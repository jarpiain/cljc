(ns org.subluminal.compiler
  (:import (java.io Reader)
           (java.nio ByteBuffer)
           (java.util IdentityHashMap Arrays List)
           (java.lang.reflect Field Method Constructor)
           (clojure.lang LineNumberingPushbackReader
                         DynamicClassLoader Reflector LazySeq IObj
                         RestFn AFunction Namespace
                         IPersistentList IPersistentVector
                         IPersistentMap IPersistentSet
                         PersistentList PersistentList$EmptyList
                         PersistentVector PersistentArrayMap PersistentHashSet
                         RT Var Symbol Keyword ISeq IFn))
  (:use (clojure.contrib monads)
        (clojure inspector)
        (org.subluminal util))
  (:require (org.subluminal [class-file :as asm] [binfmt :as bin])))

(def gen nil)
(def analyze nil)
(def eval-toplevel nil)
(def *debug-inspect* false)

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

(defn current-method [ctx]
  [(get (:index ctx) (:method ctx)) ctx])

(defn update-current-method [f & args]
  (fn [ctx]
    [nil (apply update-in ctx [:index (:method ctx)] f args)]))

(defn current-object [ctx]
  [(get (:index ctx) (:fn ctx)) ctx])

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

(defn register-constant [obj]
  (fn [ctx]
    (let [curr-obj (get (:index ctx) (:fn ctx))]
      (if-let [c (get (:constant-ids curr-obj) obj)]
        [c ctx]
        (let [c {::etype ::constant
                 :position (:position ctx)
                 :val (gensym "const__")
                 :orig obj
                 :java-type (class obj)
                 :obj (:name curr-obj)}
              curr-obj (update-in curr-obj [:constants] conj c)]
          (.put (:constant-ids curr-obj) obj c)
          [c (assoc-in ctx [:index (:fn ctx)] curr-obj)])))))

;; Like register-constant except the expression type is
;; changed to ::keyword or ::var
;; --> will be treated specially in fn invocation context

(defn register-keyword [kw]
  (fn [ctx]
    (let [curr-obj (get (:index ctx) (:fn ctx))]
      (if-let [prev (get (:keywords curr-obj) kw)]
        [prev ctx]
        (let [[c ctx] ((register-constant kw) ctx)
              k (assoc c ::etype ::keyword)]
          [k (update-in ctx [:index (:fn ctx) :keywords]
                        assoc kw k)])))))

(defn register-var [v]
  (fn [ctx]
    (let [curr-obj (get (:index ctx) (:fn ctx))]
      (if-let [prev (get (:vars curr-obj) v)]
        [prev ctx]
        (let [[c ctx] ((register-constant v) ctx)
              vv (assoc c ::etype ::var)]
          [vv (update-in ctx [:index (:fn ctx) :vars]
                         assoc v vv)])))))

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
                :clear-tag ct
                :clear-path ctx
                :clear-root ct
                :clear-kind :path
                :index (assoc (:index ctx) tag
                              (assoc m :containing-object obj)))]))))

(defn push-clear-node [kind root?]
  (fn [ctx]
    (let [tag (gensym "NN")]
      [nil
       (assoc ctx
              :parent ctx
              :clear-tag tag
              :clear-path ctx
              :clear-kind kind
              :clear-root (if root? tag (:clear-root ctx)))])))

(defn push-frame []
  (fn [ctx]
    [nil (assoc ctx :parent ctx)]))

(defn pop-frame [ctx]
  [nil (assoc (:parent ctx)
              :binding-sites (:binding-sites ctx)
              :index (:index ctx))])

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

(defn make-binding
  ([sym jtype] (make-binding sym jtype :local))
  ([sym jtype kind]
   (let [jtype (if jtype jtype Object)]
     (fn [ctx]
       (let [b {:symbol sym
                :label (gensym "BB")
                :kind kind
                :method-tag (:method ctx)
                :fn-tag (:fn ctx)
                :java-type jtype
                :clear-root (:clear-root ctx)}]
         [b (update-in ctx [:lexicals]
                       assoc sym b)])))))

(defn clear-path [b]
  (let [p (reverse (next (take-while identity (iterate :clear-path b))))]
    p))

(defn join-point [b1 b2]
  (loop [p1 (clear-path b1) p2 (clear-path b2)]
    (cond
      (not= (:clear-tag (first p1)) (:clear-tag (first p2)))
      nil

      (or (nil? (second p1)) (not= (:clear-tag (second p1))
                                   (:clear-tag(second p2))))
      (first p1)

      :else
      (recur (next p1) (next p2)))))

(defn make-binding-instance [b]
  (fn [ctx]
    (let [inst (assoc b :clear-path ctx
                        ::etype ::local-binding
                        :live (atom (not= (:clear-root b)
                                          (:clear-root ctx))))
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
                 (conj lives inst))])))

#_(defn boxing-unify [x y]
  (cond
    (= x y)
    x

    (or (nil? x) (nil? y))
    nil

    (isa? x y)
    y

    (isa? y x)
    x

    :else
    Object))

(defn valid-binding? [b ctx]
  (and
    (= (:fn-tag b) (:fn ctx))))

(defn close-over [b]
  (fn updater [ctx]
    (loop [obj (:fn ctx) ctx2 ctx same true]
      (assert obj)
      (if (= obj (:fn-tag b))
        (if same
          [b ctx2] 
          [(assoc b :kind :closed
                    :place (:name (first (current-object ctx))))             
           ctx2])
        (recur (->> obj
                 (get (:index ctx2))
                 :enclosing-method
                 (get (:index ctx2))
                 :containing-object)
               (update-in ctx2 [:index obj :closed-lexicals]
                          assoc (:label b)
                          (assoc b :kind :closed
                                 :place (:name (get :index ctx2) obj)))
               false)))))

(defn resolve-lexical [sym]
  (fn [ctx]
    (if-let [b (get (:lexicals ctx) sym)]
      ((close-over b) ctx)
      [nil ctx])))

#_(defn unify-loop [types]
  (fn updater [ctx]
    (if (#{:loop :fn} (:kind ctx))
      (assoc ctx :loop-locals
             (map #(assoc %1 :java-type %2)
                  (:loop-locals ctx)
                  types))
      (let [parent (updater (:parent ctx))]
        (assoc ctx
               :parent parent
               :loop-locals (:loop-locals parent))))))

(load "cljc/util")

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

(defmulti analyze
  "Annotate a form with information about lexical context
  required to generate bytecode"
  syncat :default ::invocation)

(defmulti gen
  "Generate bytecode from an analyzed form"
  ::etype :default ::invalid)

(defmethod gen ::invalid
  [form]
  (throw (IllegalArgumentException.
           (str "gen: invalid form " form))))

(defmulti eval-toplevel
  "Evaluate an analyzed top-level form"
  (fn [form loader] (::etype form)))

(defmethod eval-toplevel :default
  [form loader]
  (throw (Exception. (str "Can't eval " (::etype form) " form"))))

;; nil represents the null type (assignable to all reference types)
(defn has-java-class? [f]
  (not= (:java-type f) Void/TYPE))

(defn is-primitive? [^Class c]
  (and c (.isPrimitive c) (not= c Void/TYPE)))

(defn can-emit-unboxed? [f]
  (let [^Class c (:java-type f)]
    (and c (.isPrimitive c) (not= c Void/TYPE))))

(defn local-type [hinted inferred]
  (cond
    (and (nil? hinted) (nil? inferred)) Object

    (is-primitive? inferred)
    (if hinted
      (throw (Exception. "Can't type hint a local with a primitive initializer"))
      inferred)

    :else hinted))

(defn tea "Test analysis"
  ([form] (tea form :expression))
  ([form pos] (tea form pos (agent (DynamicClassLoader.))))
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
  (let [ldr (agent (DynamicClassLoader.))]
    (eval-toplevel (tea form :eval ldr) ldr)))

;;;; nil

(defmethod analyze ::null
  [form]
  (fn [ctx]
    [{::etype ::null
      :java-type nil
      :position (:position ctx)}
     ctx]))

(defn maybe-pop [pos]
  (when (= pos :statement)
    [[:pop]]))

(defmethod gen ::null
  [form]
  `([:aconst-null]
    ~@(maybe-pop (:position form))))

(defmethod eval-toplevel ::null
  [form loader]
  nil)

;;;; boolean literals

(defmethod analyze ::boolean
  [form]
  (fn [{:keys [want-unboxed] :as ctx}]
    [{::etype ::boolean
      :orig form
      :java-type (if (= want-unboxed Boolean/TYPE)
                   Boolean/TYPE
                   Boolean)
      :position (:position ctx)}
     ctx]))

(defmethod gen ::boolean
  [{:keys [orig position] :as form}]
  (if (can-emit-unboxed? form)
    `(~(if orig
         [:iconst-1]
         [:iconst-0])
      ~@(maybe-pop position))
    `(~(if orig
         [:getstatic [Boolean 'TRUE Boolean]]
         [:getstatic [Boolean 'FALSE Boolean]])
      ~@(maybe-pop position))))

;;;; Keywords, symbols, vars

(derive ::keyword ::constant)
(derive ::var ::constant)
(derive ::the-var ::constant)
(derive ::boolean ::constant)

(defmethod eval-toplevel ::constant
  [{:keys [orig]} loader]
  orig)

(defmethod analyze ::keyword
  [kw]
  (register-keyword kw))

(defmethod analyze ::symbol
  [sym]
  (run
    [unbox-type (fetch-val :want-unboxed)
     lex (resolve-lexical sym)
     pos (fetch-val :position)
     bind (if lex
            (make-binding-instance lex)
            (lookup-sym *ns* sym))]
    (condp = (::etype bind)
      ::local-binding
      (let [btype (:java-type bind)
            hint (tag-class *ns* (tag-of sym))
            res (assoc bind :position pos)]
        (cond
          (true? unbox-type)
          res
          (and (is-primitive? btype) (not= btype unbox-type))
          (assoc res :java-type Number :unbox-type btype)
          :else res))

      ::static-field
      (throw (Exception. "static-field is unimplemented"))

      ::var bind
      ::constant bind)))

(defmethod eval-toplevel ::var
  [{:keys [orig]} loader]
  (deref orig))

(defmethod gen ::var
  [{:keys [position val obj]}]
  `([:getstatic [~obj ~val ~Var]]
    [:invokevirtual ~[Var 'get [:method Object []]]]
    ~@(maybe-pop position)))

(defmethod analyze [::special 'var]
  [[_ vname :as form]]
  (let [v (lookup-var *ns* vname false)]
    (if v
      (run [cv (register-var v)]
        (assoc cv ::etype ::the-var))
      (throw (Exception. (str "Unable to resolve var: " vname
                              " in this context"))))))

;;;; Number

(defn unboxed-type [^Class c]
  (cond 
    (= c Long) Long/TYPE
    (= c Integer) Long/TYPE
    (= c Double) Double/TYPE))

(derive ::number ::constant)
(defmethod analyze ::number
  [x]
  (let [x (if (instance? Integer x) (long x) x)]
    (if (or (instance? Long x)
            (instance? Double x))
      (fn [ctx]
        (let [pos (:position ctx)
              jt (:want-unboxed ctx)]
          (if (or (= jt (unboxed-type (class x)))
                  (true? jt))
            [{::etype ::number
              :java-type (unboxed-type (class x))
              :position pos
              :orig x} ctx]
            ((register-constant x) ctx))))
      (register-constant x))))

(defmethod gen ::number
  [{:keys [position orig]}]
  ;; Only long and double arithmetic supported
  `([:ldc2-w ~orig]
    ~@(maybe-pop position)))

;;;; Other constants

(defmethod analyze ::constant
  [obj]
  (register-constant obj))

(defmethod gen ::constant
  [{:keys [position val obj java-type]}]
  `([:getstatic [~obj ~val ~java-type]]
    ~@(maybe-pop position)))

(defn load-op [jt]
  (cond
    (= jt Long/TYPE) :lload
    (= jt Double/TYPE) :dload
    :else :aload))

(defn store-op [jt]
  (cond
    (= jt Long/TYPE) :lstore
    (= jt Double/TYPE) :dstore
    :else :astore))

(defn return-op [jt]
  (cond
    (= jt Long/TYPE) :lreturn
    (= jt Double/TYPE) :dreturn
    :else :areturn))

(defmethod analyze [::special 'quote]
  [[_ obj]]
  (register-constant obj))

;; using :iconst-0 :istore
;; instead of :aconst-null :astore
;; This way the bytecode verifier will catch too aggressive locals clearing
(defn maybe-clear-local
  "Generate bytecode to clear a local binding after use"
  [{:keys [kind java-type unbox-type label live]}]
  (cond
    (or (is-primitive? java-type) (is-primitive? unbox-type))
    nil
    (= kind :closed) nil
    ; (this?) nil
    @live nil
    :else (list [:iconst-0] [:istore label])))

(defmethod gen ::local-binding
  [{:keys [kind position java-type unbox-type place label] :as lb}]
  `(~@(if (= kind :closed)
        (list [:aload-0] [:getfield [place label java-type]])
        (list [(load-op (or unbox-type java-type)) label]))
    ~@(maybe-clear-local lb)
    ~@(when (and unbox-type (not (is-primitive? java-type)))
        (condp = unbox-type
          Long/TYPE
          `([:invokestatic ~[Long 'valueOf [:method Long [:long]]]])
          Double/TYPE
          `([:invokestatic ~[Double 'valueOf [:method Double [:double]]]])))
    ~@(maybe-pop position)))

;;;; Empty collections

(derive ::empty ::constant)

(defmethod analyze ::empty
  [coll]
  ;; TODO metadata...
  (run [pos (fetch-val :position)]
    {::etype ::empty
     :java-class (class coll)
     :position pos
     :orig coll}))

(defmethod gen ::empty
  [{:keys [orig position]}]
  `(~(cond
       (instance? IPersistentList orig)
       [:getstatic [PersistentList 'EMPTY PersistentList$EmptyList]]
       (instance? IPersistentVector orig)
       [:getstatic [PersistentVector 'EMPTY PersistentVector]]
       (instance? IPersistentMap orig)
       [:getstatic [PersistentArrayMap 'EMPTY PersistentArrayMap]]
       (instance? IPersistentSet orig)
       [:getstatic [PersistentHashSet 'EMPTY PersistentHashSet]]
       :else (throw (UnsupportedOperationException.
                       "Unknown collection type")))
    ~@(maybe-pop position)))

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
        entries (m-map analyze (flatten (seq entries)))
        _ (set-val :position pos)
        res (if (every? #(isa? (::etype %) ::constant) entries)
              (register-constant (apply array-map (map :orig entries)))
              (m-result {::etype ::map
                         :entries entries
                         :java-class IPersistentMap
                         :position pos}))]
       res))

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

;;;; fn invocation

(defmethod analyze ::invocation
  [[op & args :as form]]
  (run
    [me (macroexpand-impl *ns* form)
     res (if-not (identical? me form)
           (analyze me)
           (run [pos (update-val :position #(if (= % :eval) :eval :expression))
                 jt (set-val :want-unboxed nil)
                 op (analyze op)
                 ;; special case: instanceof
                 ;; special case: keyword invoke, static invoke
                 args (m-map analyze args)
                 _ (set-val :position pos)]
                {::etype ::invocation
                 :op op
                 :args (doall args)
                 :position pos
                 :java-type Object}))]
    res))

(defmethod gen ::invocation
  [{:keys [op args position java-type]}]
  `(~@(gen op)
    [:checkcast ~IFn]
    ~@(mapcat gen args)
    [:invokeinterface ~[IFn 'invoke [:method Object (take (count args)
                                                          (repeat Object))]]]
    ~@(maybe-pop position)))

(defmethod eval-toplevel ::invocation
  [{:keys [op args]} loader]
  (let [eop (eval-toplevel op loader)
        eargs (doall (map #(eval-toplevel % loader) args))]
    (apply eop eargs)))

;;;; Assignment

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
      (if (isa? (etype target) ::assignable)
        (with-meta `(~'set! ~target ~value)
                   {::etype ::set!
                    :position pos})
        (throw (IllegalArgumentException. "Invalid assignment target"))))))

;;;;

(defmethod analyze [::special 'recur]
  [[_ & inits :as form]]
  (fn [{:keys [position loop-locals loop-label catching] :as ctx}]
    (cond
      (or (not= position :return) (nil? loop-label))
      (throw (Exception. "Can only recur from tail position"))

      catching
      (throw (Exception. "Cannot recur from catch/finally"))

      (not (== (count inits) (count loop-locals)))
      (throw (Exception.
        (format "Mismatched argument count to recur, expected: %d args, got %d"
                (count loop-locals)
                (count inits))))

      :valid
      (let [[inits ctx]
            (run-with ctx
              [_ (set-val :want-unboxed true)
               is (m-map analyze inits)]
              is)]
        [{::etype ::recur
          :position position ; redundant
          :inits inits
          :loop-label loop-label
          :loop-locals loop-locals}         
         ctx]))))

(defmethod gen ::recur
  [{:keys [inits loop-label loop-locals]}]
  `(~@(mapcat (fn [i lb]
                (let [have-type (:java-type i)
                      need-type (:java-type lb)
                      doinit (cond
                               (= have-type need-type)
                               (gen i)
                               :else
                               (do (println "have type" have-type
                                            "need type" need-type)
                                 nil))]
                  `(~@doinit
                    ~[(store-op need-type) (:label lb)])))
              inits loop-locals)
    [:goto ~loop-label]))

;;;; Sequencing

(defmethod analyze [::special 'do]
  [[_ & body]]
  (cond
    (empty? body)
    (analyze nil)

    (empty? (next body))
    (analyze (first body))

    :else
    (run
      [pos (update-val :position #(if (= % :eval) :eval :statement))
       jt (set-val :want-unboxed nil)
       stmts (m-map analyze (butlast body))
       _ (set-val :position pos)
       _ (set-val :want-unboxed jt)
       tail (analyze (last body))]
      {::etype ::do
       :body (doall (concat stmts [tail]))
       :position pos})))

(defmethod gen ::do
  [{:keys [body]}]
  (mapcat gen body))

(defmethod eval-toplevel ::do
  [{:keys [body]} loader]
  (dorun (map #(eval-toplevel % loader) (butlast body)))
  (eval-toplevel (last body) loader))

;;;; Conditional

(defn common-superclass [l r]
  (cond
    (isa? l r) r
    (isa? r l) l
    :else Object)) ; give up

(defmethod analyze [::special 'if]
  [[_ test-expr then else :as form]]
  (cond
    (> (count form) 4)
    (throw (Exception. "Too many arguments to if"))

    (< (count form) 3)
    (throw (Exception. "Too few arguments to if"))

    :else
    (run
      [pos (fetch-val :position) ; redundant?
       jt (set-val :want-unboxed Boolean/TYPE)
       test-expr (analyze test-expr)
       _ (push-clear-node :branch false)
       _ (push-clear-node :path false)
       _ (set-val :want-unboxed jt)
       then (analyze then)
       _ pop-frame
       _ (push-clear-node :path false)
       _ (set-val :want-unboxed jt)
       else (analyze else)
       _ pop-frame
       _ pop-frame]
      (let [left (:java-type then)
            right (:java-type else)
            jt (cond
                 (= left right)
                 left

                 (or (is-primitive? left) (is-primitive? right))
                 Object

                 :else
                 (common-superclass left right))]
        {::etype ::if
         :position pos
         :java-type jt
         :test test-expr
         :then then
         :else else}))))

(defn gen-boxed [orig-type form]
  (cond
    (= orig-type Boolean/TYPE)
    (let [[falsel endl] (map gensym ["False__ " "End__"])]
    `(~@(gen form)
      [:ifeq ~falsel]
      [:getstatic ~[Boolean 'TRUE Boolean]]
      [:goto ~endl]
      [:label ~falsel]
      [:getstatic ~[Boolean 'FALSE Boolean]]
      [:label ~endl]))
    (= orig-type Long/TYPE)
    `(~@(gen form)
      [:invokestatic ~[Long 'valueOf [:method Long [Long/TYPE]]]])
    (= orig-type Double/TYPE)
    `(~@(gen form)
      [:invokestatic ~[Double 'valueOf [:method Double [Double/TYPE]]]])
    :else (gen form)))

(defmethod gen ::if
  [{:keys [position java-type test then else]}]
  (let [lt (:java-type then) rt (:java-type else)
        coerce-left (if (= lt java-type)
                      (gen then)
                      (gen-boxed lt then))
        coerce-right (if (= rt java-type)
                       (gen else)
                       (gen-boxed rt else))]
    (if (can-emit-unboxed? test)
      (let [[falsel endl] (map gensym ["False__" "End__"])]
        `(~@(gen test)
          [:ifeq ~falsel]
          ~@coerce-left
          [:goto ~endl]
          [:label ~falsel]
          ~@coerce-right
          [:label ~endl]))
      (let [[null falsel endl] (map gensym ["Null__" "False__" "End__"])]
        `(~@(gen test)
          [:dup]
          [:ifnull ~null]
          [:getstatic ~[Boolean 'FALSE Boolean]]
          [:if-acmpeq ~falsel]
          ~@coerce-left
          [:goto ~endl]
          [:label ~null]
          [:pop]
          [:label ~falsel]
          ~@coerce-right
          [:label ~endl])))))

(defmethod eval-toplevel ::if
  [{:keys [test then else]} loader]
  (let [stat (eval-toplevel test loader)]
    (if stat
      (eval-toplevel then loader)
      (eval-toplevel else loader))))

;;;; Let & loop

(declare analyze-loop)

(defmethod analyze [::special 'let*]
  [form]
  (analyze-loop form false))

(defmethod analyze [::special 'loop]
  [form]
  (analyze-loop form true))

(defn analyze-init
  [[sym init]]
  (let [hint (when (:tag (meta sym)) (tag-class *ns* (tag-of sym)))]
    (cond
      (not (symbol? sym))
      (throw (IllegalArgumentException.
               (str "Bad binding form, expected symbol, got: " sym)))

      (namespace sym)
      (throw (IllegalArgumentException.
               (str "Can't let qualified name: " sym)))

      :else
      (run
        [_ (set-val :want-unboxed true)
         init (analyze init)
         lb (make-binding sym (local-type hint (:java-type init)))]
        [lb init]))))

(defn analyze-loop0
  [bindings body loop?]
  (let [loop-label (gensym "LOOP__")]
    (run
      [_ (push-frame)
       pos (fetch-val :position)
       jt (fetch-val :want-unboxed)
       bindings (m-map analyze-init (partition 2 bindings))
       _ (set-val :loop-locals (map first bindings))
       _ (if loop?
           (set-vals :position :return :loop-label loop-label)
           (m-result nil))
       _ (if loop? (push-clear-node :path true) (m-result nil))
       _ (set-val :want-unboxed jt)
       body (analyze `(~'do ~@body))
       _ (if loop? pop-frame (m-result nil))
       _ pop-frame]
      {::etype (if loop? ::loop ::let)
       :position pos
       :java-type (:java-type body)
       :bindings bindings
       :loop-label loop-label
       :body body})))

(defn analyze-loop
  [[_ bindings & body :as form] loop?]
  (cond
    (not (vector? bindings))
    (throw (IllegalArgumentException. "Bad binding form, expected vector"))

    (odd? (count bindings))
    (throw (IllegalArgumentException.
             "Bad binding form, expected matching symbol expression pairs"))

    :else
    (run
      [pos (fetch-val :position)
       r (if (or (= pos :eval)
                 (and loop? (= pos :expression)))
           (analyze `((fn* [] ~form)))
           (analyze-loop0 bindings body loop?))]
      r)))

(defmethod gen ::let
  [{:keys [java-type bindings body]}]
  (let [lbs (map first bindings)
        block-init (mapcat (juxt :label :java-type) lbs)]
    `((:block nil nil ~block-init
        ~@(mapcat
            (fn [[lb init]]
              `(~@(gen init)
                ~[(store-op (:java-type lb)) (:label lb)]))
            bindings)
        ~@(gen body)))))

(defmethod gen ::loop
  [{:keys [java-type bindings body loop-label]}]
  (let [lbs (map first bindings)
        block-init (mapcat (juxt :label :java-type) lbs)]
    `((:block nil nil ~block-init
        ~@(mapcat
            (fn [[lb init]]
              `(~@(gen init)
                ~[(store-op (:java-type lb)) (:label lb)]))
            bindings)
        [:label ~loop-label]
        ~@(gen body)))))

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
                  :this-name this-name ; symbol used for self-recursion
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
(defn arity [m]
  (count (:required-params (:argv m))))

(defmethod analyze [::special 'fn*]
  [[op & opts :as form]]
  (run
    [classloader (fetch-val :loader)
     pos (fetch-val :position)
     [op & meth :as norm] (normalize-fn* form)
     _ (push-object-frame (meta norm))
     meth (m-map analyze-method meth)
     f current-object
     _ pop-frame
     ;; was: resolve-lexical
     initargs (m-map analyze (map :symbol (vals (:closed-lexicals f))))]
    (loop [a (vec (repeat (inc +max-positional-arity+) nil))
           variadic nil
           remain meth]
      (if (seq remain)
        (let [[info body :as m] (first remain)
              ar (arity info)]
          (if (variadic? (:argv info))
            (if variadic
              (throw (Exception. "Can't have more than 1 variadic overload"))
              (recur a m (next remain)))
            (if (a ar)
              (throw (Exception. "Can't have 2 overloads with same arity"))
              (recur (assoc a ar m) variadic (next remain)))))
        (do
          (when-let [[info _] variadic]
            (let [v-ar (arity info)]
              (when (some identity (subvec a (inc v-ar)))
                (throw (Exception.
                         (str "Can't have fixed arity function"
                              " with more params than variadic function"))))))
          (when (and (:static? f)
                     (not (empty? (:closed-lexicals f))))
            (throw (Exception. "Static fns can't be closures")))
          ;; compile & load
          ;; add metadata from original form
          (let [obj (assoc f
                           :methods (filter identity a)
                           :variadic-method variadic)]
            ;(compile-class @classloader obj (if variadic RestFn AFunction) [])
            (send classloader (bound-fn [& args] (apply compile-class args))
                  obj (if variadic RestFn AFunction) [])
            (try
              (await-for 1000 classloader)
              (if-let [e (agent-error classloader)]
                (throw e)))
            {::etype ::fn
             :class-name (:name obj)
             :initargs initargs
             :position pos}))))))

(defmethod gen ::fn
  [{:keys [class-name initargs position]}]
  `([:new ~class-name]
    [:dup]
    ~@(apply concat
        (for [b initargs]
          (gen b)))
    [:invokespecial ~[class-name '<init>
                      [:method :void (map :java-type initargs)]]]
    ~@(maybe-pop position)))

;(gen (assoc b ::etype ::local-binding :position :expression))

(defmethod eval-toplevel ::fn
  [{:keys [class-name initargs]} loader]
  (await loader)
  (let [^Class c (.loadClass @loader (str class-name))
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
      (run
        [{:keys [this-name static?] :as this-fn} (fetch-val :fn)
         argv (m-result (process-fn*-args argv static?))
         _ (push-method-frame {:loop-label (gensym "LL")
                               :loop-locals nil})
         thisb (if static?
                 (m-result nil)
                 (make-binding (or this-name (gensym "THIS")) IFn :fnarg))
         bind (m-map (fn [[s t]] (make-binding s t :fnarg))
                     (if (variadic? argv)
                       (conj (:required-params argv)
                             (:rest-param argv))
                       (:required-params argv)))
         _ (update-current-method assoc :loop-locals bind)
         _ (set-val :loop-locals bind)
         body (analyze `(~'do ~@body))
         m current-method
         _ pop-frame]
        [(assoc m
                :argv argv
                :bind bind
                :this thisb)
                body]))))

(derive ::if ::maybe-primitive)
(derive ::let ::maybe-primitive)
(derive ::do ::maybe-primitive)
(derive ::loop ::maybe-primitive)

(derive ::null ::literal)
(derive Boolean ::literal)
(derive String ::literal)

(derive Boolean ::primitive)
(derive ::primitive ::maybe-primitive)

(comment
(defn analyze-seq
  [ctx form name]
  ;; (binding [*line* ...])
  (let [mac (macroexpand1 form)]
    (if-not (identical? mac form)
      (analyze ctx mac name)
      (let [op (first form)]
        (cond
          (nil? op)
          (throw (IllegalArgumentException. "Can't call nil"))

          ;; inline

          (= op 'fn*)
          (analyze-fn ctx form name)

          (contains? +specials+ op)
          (analyze-special ctx form)

          :else
          (with-meta
            (map (partial analyze ctx) form)
            {::etype ::invoke}))))))

;;;; String literal

(defmethod literal-val String
  [form]
  form)

(defmethod gen String
  [form ctx]
  `([:ldc ~form]
    ~@(when (= ctx :statement)
        [[:pop]])))
)

;; Should change to defmulti
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
      [:invokestatic ~[double 'valueOf [:method Double [:double]]]])
    (string? c)
    `([:ldc ~c])

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
    `(~@(gen-constant-array (flatten (seq c)))
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
       ~[RT 'vector [:method IPersistentVector [[:array Object]]]]])))

(defn emit-constants [obj cref mref]
  (doseq [{:keys [val java-type obj orig]} (:constants obj)]
    (asm/emit cref mref
      `(~@(gen-constant orig)
        [:checkcast ~java-type]
        [:putstatic [~obj ~val ~java-type]]))))

;; TODO: defmulti --> deftypes also
(defn emit-methods [obj cref]
  (doseq [[info body :as mm] (:methods obj)]
    (let [{:keys [argv bind this loop-label]} info] 
      (let [mref (asm/add-method cref
                   {:name 'invoke
                    :descriptor [:method Object
                                 (repeat (count bind) Object)]
                    :params (map :label bind)
                    :flags #{:public}
                    :throws [Exception]})]
        (asm/emit1 cref mref [:label loop-label])
        (asm/emit cref mref (gen body))
        (asm/emit1 cref mref [:areturn])
        (asm/assemble-method mref)))))

(defn write-class-file [& more])

(defn compile-class
  [loader obj super ifaces]
  (asm/assembling [c {:name (:name obj)
                      :extends super
                      :implements ifaces
                      :flags #{:public :super :final}}]
    (doseq [fld (:constants obj)]
      (let [{:keys [val java-type]} fld]
        (asm/add-field c {:name val
                          :descriptor java-type
                          :flags #{:public :final :static}})))

    (let [clinit (asm/add-method c {:name '<clinit>
                                    :descriptor [:method :void []]
                                    :flags #{:public :static}})]
      (emit-constants obj c clinit)
      (asm/emit1 c clinit [:return])
      (asm/assemble-method clinit))

    (doseq [[sym bind] (:closed-lexicals obj)]
      (let [{:keys [java-type]} bind]
        (asm/add-field c {:name sym
                          :descriptor java-type
                          :flags #{:public :final}})))

    (let [clos (:closed-lexicals obj)
          clos-names (keys clos)
          clos-types (map :java-type (vals clos))]
      (let [init (asm/add-method c {:name '<init>
                                    :descriptor [:method :void clos-types]
                                    :params clos-names
                                    :flags #{:public}})]
        (asm/emit c init
          `([:aload-0]
            [:invokespecial ~[super '<init> [:method :void []]]]
            ~@(mapcat (fn [arg typ]
                        `([:aload-0]
                          [:aload ~arg]
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

    (let [with-meta (asm/add-method c {:name 'with-meta
                                       :descriptor
                                         [:method IObj [IPersistentMap]]
                                       :flags #{:public}})]
      (asm/emit c with-meta
        `([:aload-0]
          [:areturn]))
      (asm/assemble-method with-meta))

    (emit-methods obj c)
    (asm/assemble-class c)

    (let [bytecode (ByteBuffer/allocate 10000)] 
      (bin/write-binary ::asm/ClassFile bytecode c)
      (let [siz (.position bytecode)
            arr (byte-array siz)]
        (.flip bytecode)
        (.get bytecode arr)
        (.clear bytecode)
        (try
          (.defineClass loader (str (:name obj)) arr nil)
          (when *debug-inspect*
            (inspect-tree (bin/read-binary ::asm/ClassFile bytecode)))
          (catch Throwable e
            (println "Caught while loading class:" (class e) e)))))
    loader))

;;;; toplevel

;; A constant is
;; ^{::etype ::constant} {:type foo :val v}
(comment
  ([rd src-path src-name]
  (binding [*source-path* src-path
            *source-file* src-name
            *method* nil
            *local-env* {} ; symbol -> LocalBinding
            *loop-locals* nil
            *ns* *ns*
            *constants* []
            *constant-ids* {} ;; IdentityHashMap.
            *keywords* {}
            *vars* {}]
    (assembling [ns-init {:name (file->class-name src-path)
                          :flags #{:public :super}
                          :source src-name}]
      (let [loader (add-method ns-init {:name 'load
                                        :descriptor [:method :void []]})
            eof (Object.)]
        (loop []
          (let [form (read rd nil eof)]
            (when-not (identical? form eof)
              (emit ns-init loader (compile1 form))
              (recur))))
        (assemble-method m))

      (doseq [i (range (count (:constants @init-ns)))]
        (add-field ns-init {:name (constant-name i)
                            :flags #{:public :static :final}}))

      ;; TODO inits in blocks of 100...

      (let [init-const (add-method ns-init {:name '__init0
                                            :descriptor [:method :void []]
                                            :flags #{:public :static}})]
        (binding [*print-dup* true]
          (doseq [c (:constants @ns-init)]
              (emit init-ns init-const
                `(~@(gen-constant c)
                  [:checkcast ~(:type c)]
                  [:putstatic (:name @ns-init) (constant-name i) (:type c)]))))
        (emit1 init-ns init-const [:return])
        (assemble-method init-const))

      (let [clinit (add-method ns-init {:name '<clinit>
                                        :descriptor [:method :void []]
                                        :flags #{:public :static}})
            [start-try end-try end final] (take 4 (repeatedly gensym))]
        (emit init-ns clinit
          `([:iconst-2] ; inlined Compiler/pushNS()
            [:anewarray [:array ~Object]]
            [:dup]
            [:iconst-0] ; arr arr 0
            [:ldc "clojure.core"]
            [:invokestatic [~Symbol "create" [:method ~Symbol [~String]]]]
            [:ldc "*ns*"]
            [:invokestatic [~Symbol "create" [:method ~Symbol [~String]]]]
            [:invokestatic [~Var "intern" [:method ~Var [~Symbol ~Symbol]]]]
            [:aastore] ; arr[var,null]
            [:invokestatic [~PersistentHashMap "create"
                            [:method ~PersistentHashMap [[:array Object]]]]]
            [:invokestatic [~Var ~'pushThreadBindings
                            [:method :void [~Associative]]]]

            [:label ~start-try]
            [:invokestatic (:name @init-ns) 'load [:method :void []]]
            [:label ~end-try]
            [:invokestatic [~Var ~'popThreadBindings [:method :void []]]]
            [:goto ~end]
            [:label ~final]
            [:invokestatic [~Var ~'popThreadBindings [:method :void []]]]
            [:athrow]
            [:label ~end]
            [:return]
            [:catch ~start-try ~end-try ~final nil]))
        (assemble-method clinit))
      (assemble-class ns-init)
      (sendout @ns-init *compile-path*))))

(defn compile1
  "Compile a top-level form"
  [form]
  (let [form (macroexpand-impl form)]
    (if (and (seq? form) (= (first form) 'do))
      (doall (apply concat (map compile1 (next form))))
      (let [tagged (analyze form :eval)
            bytecode (gen tagged :expression)]
        (eval-impl tagged)
        bytecode)))))
