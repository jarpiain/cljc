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
        [(assoc c :position (:position ctx)) ctx]
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
        [(assoc prev :position (:position ctx)) ctx]
        (let [[c ctx] ((register-constant kw) ctx)
              k (assoc c ::etype ::keyword)]
          [k (update-in ctx [:index (:fn ctx) :keywords]
                        assoc kw k)])))))

(defn register-var [v]
  (fn [ctx]
    (let [curr-obj (get (:index ctx) (:fn ctx))]
      (if-let [prev (get (:vars curr-obj) v)]
        [(assoc prev :position (:position ctx)) ctx]
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
                :catching nil
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
                :force-live (when lbl true) ; don't clear 'this'
                :kind kind
                :method-tag (:method ctx)
                :fn-tag (:fn ctx)
                :java-type jtype
                :clear-root (:clear-root ctx)}]
         [b (update-in ctx [:lexicals]
                       assoc sym b)])))))

;; the assembler recognizes 'this as a special label
(defn make-this-binding
  [sym jtype]
  (make-binding sym jtype :fnarg 'this))


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

(defn make-binding-instance [b]
  (fn [ctx]
    (let [inst (assoc b :clear-path ctx
                        ::etype ::local-binding
                        :live (atom (or (:force-live b)
                                        (not= (:clear-root b)
                                              (:clear-root ctx)))))
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

(defn valid-binding?
  "Return true if b is a local in the current method
  (not closed-over)"
  [b ctx]
  (= (:fn-tag b) (:fn ctx)))

(defn close-over
  "Make the current method a closure over b"
  [b]
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

(defn resolve-lexical
  "Return the visible lexical binding of sym if any.
  The current method will be made closure over the binding"
  [sym]
  (fn [ctx]
    (if-let [b (get (:lexicals ctx) sym)]
      ((close-over b) ctx)
      [nil ctx])))

(load "cljc/util")

(defn syncat
  "The syntax category of a form. Dispatch function for analyze."
  [x]
;  {:post [(do (println "-->" %) true)]}
;  (print "::analyzing" x)
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
  (fn [form ^ClassLoader loader] (::etype form)))

(defmethod eval-toplevel :default
  [form loader]
  (throw (Exception. (str "Can't eval " (::etype form) " form"))))

;; nil represents the null type (assignable to all reference types)
;; Using Void/TYPE for UntypedExprs though this doesn't seem to be
;; very useful
(defn has-java-class? [f]
  (not= (:java-type f) Void/TYPE))

(defn is-primitive? [^Class c]
  (and c (.isPrimitive c) (not= c Void/TYPE)))

(defn can-emit-unboxed? [f]
  (is-primitive? (:java-type f)))

(defn- local-type
  "Decide the type of a let-local in case
  there is both a type hint and inferred type
  for the initializer expression"
  [hinted inferred]
  (cond
    (and (nil? hinted) (nil? inferred)) Object

    (is-primitive? inferred)
    (if hinted
      (throw (Exception. "Can't type hint a local with a primitive initializer"))
      inferred)

    :else hinted))

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

;;;; nil

(defmethod analyze ::null
  [form]
  (fn [ctx]
    [{::etype ::null
      :java-type nil
      :position (:position ctx)}
     ctx]))

;; FIXME: breaks if there are primitive longs and doubles
;; in :statement position
(defn maybe-pop
  "Emit the instruction to pop an unused result
  from the operand stack"
  ([pos] (maybe-pop pos Object))
  ([pos jtype]
   (when (and (= pos :statement)
              (not= jtype Void/TYPE))
     (if (or (= jtype Long/TYPE)
             (= jtype Double/TYPE))
       [[:pop2]]
       [[:pop]]))))

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

;;;; String literals

(defmethod analyze ::string
  [^String form]
  (register-constant (.intern form)))

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
      (assoc bind :position pos
                  :tag (tag-of sym))

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
        (list [:aload-0] [:getfield [place label (or unbox-type java-type)]])
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
     :orig (if (instance? LazySeq coll) () coll)}))

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
        entries (m-map analyze (apply concat (seq entries)))
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

;;;; Exception handling

(defmethod analyze [::special 'throw]
  [[_ ex :as form]]
  (fn [ctx]
    (if (= (:position ctx) :eval)
      ((analyze `((~'fn* [] ~form))) ctx)
      (run-with ctx
        [pos (set-val :position :expression)
         _ (set-val :want-unboxed nil)
         exc (analyze ex)
         _ (set-val :position pos)]
        {::etype ::throw
         :position pos
         :java-type Void/TYPE
         :exc exc}))))

(defmethod gen ::throw
  [{:keys [exc]}]
  `(~@(gen exc)
    [:checkcast ~Throwable]
    [:athrow]))

(defn- catch-clause? [form]
  (and (seq? form)
       (contains? #{'catch 'finally} (first form))))

(defn analyze-catch-clause
  [unbox? [op & args]]
  (condp = op
    'catch
    (let [[etyp evar & body] args
          eclass (maybe-class *ns* etyp false)]
      (cond
        (nil? eclass)
        (throw (IllegalArgumentException.
                 (str "Unable to resolve classname: " etyp)))
        (not (symbol? evar))
        (throw (IllegalArgumentException.
                 (str "Bad binding form, expected symbol, got: " evar)))
        (namespace evar)
        (throw (Exception.
                 (str "Can't bind qualified name: " evar))))
      (run [_ (push-frame)
            lb (make-binding evar eclass)
            _ (set-vals :catching true :want-unboxed unbox?)
            body (analyze `(~'do ~@body))
            _ pop-frame]
        {::etype ::catch
         :jtype (:java-type body)
         :bind lb
         :body body}))

    'finally
    (run [_ (push-frame)
          _ (set-vals :catching true :position :statement
                      :want-unboxed nil)
          body (analyze `(~'do ~@args))
          _ pop-frame]
      {::etype ::finally
       :body body})))

(defn- ensure-primitive
  [^Class cls]
  (when (and cls (.isPrimitive cls) cls)))

(defmethod analyze [::special 'try]
  [[_ & body :as form]]
  (fn [ctx]
    (if (not= (:position ctx) :return)
      ((analyze `((~'fn* [] ~form))) ctx)
      (let [[body clauses] (split-with (complement catch-clause?) body)]
        (loop [remain clauses]
          (when (seq remain)
            (cond
              (not (catch-clause? (first remain)))
              (throw (Exception. (str "Only catch or finally clause"
                                      " can follow catch in try expression")))
              (and (= (ffirst remain) 'finally) (next remain))
              (throw (Exception. (str "Finally clause must be last"
                                      " in try expression"))))
            (recur (next remain))))
        (run-with ctx
          [pos (fetch-val :position)
           body (analyze `(~'do ~@body))
           clauses (m-map (partial analyze-catch-clause
                                   (ensure-primitive (:java-type body)))
                          clauses)]
          (let [final (last clauses)
                final (when (= (::etype final) ::finally) final)]
            {::etype ::try
             :java-type (:java-type body)
             :body body
             :position pos
             :clauses (if final (butlast clauses) clauses)
             :final final}))))))

(defn gen-catch [ret-local retl start-try end-try finallyl
                 {:keys [java-type position final]}
                 {:keys [bind jtype body]}]
  (let [eclass (:java-type bind)
        hstart (gensym "HSTART__")
        hend (gensym "HEND__")]
    `((:block ~hstart ~hend ~[(:label bind) (:java-type bind)]
        [:astore ~(:label bind)]
        ~@(gen body)
        ~@(when (not= position :statement)
            (list [(store-op java-type) ret-local])))
      ~@(when final
          (gen (:body final)))
      [:goto ~retl]
      [:catch ~start-try ~end-try ~hstart ~eclass]
      ~@(when final (list [:catch hstart hend finallyl nil])))))

(defmethod gen ::try
  [{:keys [java-type position body clauses final] :as whole}]
  (let [[start-try end-try endl retl finallyl ret-local]
        (map gensym ["TRY__" "END_TRY__" "END__" "RET__" "FIN__"
                     "RETLOC"])]
    `((:block ~start-try ~endl ~[ret-local java-type]
        ~@(gen body)
        ~@(when (not= position :statement)
            (list [(store-op java-type) ret-local]))
        [:label ~end-try]
        ~@(when final
            (gen (:body final)))
        [:goto ~retl]
        ~@(mapcat (partial gen-catch ret-local retl start-try
                           end-try finallyl whole)
                  clauses)
        ~@(when final
            (let [finalarg (gensym "FVAL")]
              `((:block ~finallyl nil [~finalarg ~Throwable]
                  [:astore ~finalarg]
                  ~@(gen (:body final))
                  [:aload ~finalarg]
                  [:athrow]))))
        [:label ~retl]
        ~@(when (not= position :statement)
            (list [(load-op java-type) ret-local]))))))

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

(declare coerce-primitive)
(defn gen-typed-arg [typ arg]
  (let [atyp (:java-type arg)
        acode (gen arg)]
    (cond
      (= typ arg)
      acode
      (not (.isPrimitive typ))
      `(~@acode
        [:checkcast ~typ])
      :else
      `(~@acode
        ~@(coerce-primitive typ)))))

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
  (let [argvals (into-array (map #(eval-toplevel % loader) args))]
    (if ctor
      (.newInstance ctor
                    (box-args (.getParameterTypes ctor) argvals))
      (Reflector/invokeConstructor java-type argvals))))

(defn analyze-java-method [form ^Class c inst member args unboxed?]
  (let [sym (if (symbol? member)
              member
              (symbol (name member)))
        tag (tag-of form)]
    (if c
      ;; static
      (let [methods (Reflector/getMethods c (count args) (name sym) true)
            ^Method the-method
            (cond
              (empty? methods)
              (throw (Exception. (str "No matching method: " member)))
              (== (count methods) 1)
              (first methods)
              :else
              (matching-method (map :java-type args) methods))
            typ (if the-method (.getReturnType the-method) Object)
            source-type (if tag (tag-class *ns* tag) typ)
            target-type (if (and (.isPrimitive source-type)
                                 (not (true? unboxed?))
                                 (not= source-type unboxed?))
                          Object
                          source-type)]
        {::etype ::static-method
         :target c
         :args args
         :method the-method
         :member member
         :java-type target-type
         :unbox-type source-type
         :tag tag})
      ;; virtual
      (let [methods (Reflector/getMethods (or (:java-type inst) Object)
                                          (count args) (name sym) false)
            ^Method the-method
            (cond
              (empty? methods) nil
              (== (count methods) 1)
              (first methods)
              :else
              (matching-method (map :java-type args) methods))
            typ (if the-method (.getReturnType the-method) Object)
            source-type (if tag (tag-class *ns* tag) typ)
            target-type (if (and (.isPrimitive source-type)
                                 (not (true? unboxed?))
                                 (not= source-type unboxed?))
                          Object
                          source-type)]
        {::etype ::instance-method
         :target inst
         :target-class (:java-type inst)
         :args args
         :method the-method
         :member member
         :java-type target-type
         :unbox-type source-type
         :tag tag}))))

(defn analyze-field [form ^Class c inst member unboxed?]
  (let [sym (if (symbol? member)
              member
              (symbol (name member)))
        tag (tag-of form)]
    (if c
      (let [^Field fld (.getField c (munge-impl (name sym)))
            typ (.getType fld)
            source-type (if tag (tag-class *ns* tag) typ)
            target-type (if (and (.isPrimitive source-type)
                                 (not (true? unboxed?))
                                 (not= source-type unboxed?))
                          Object
                          source-type)]
        {::etype ::static-field         
         :target c
         :member fld
         :java-type target-type
         :unbox-type source-type
         :tag tag})
      (let [^Class c (when (and inst (:java-type inst)
                                (not (.isPrimitive (:java-type inst))))
                       (:java-type inst))
            ^Field fld (when c (Reflector/getField
                                 c (munge-impl (name sym)) false))
            typ (if fld (.getType fld) Object)
            source-type (if tag (tag-class *ns* tag) typ)
            target-type (if (and (.isPrimitive source-type)
                                 (not (true? unboxed?))
                                 (not= source-type unboxed?))
                          Object
                          source-type)]
      {::etype ::instance-field
       :target-class c
       :target inst
       :field fld
       :member (munge-impl (name sym))
       :java-type target-type
       :unbox-type source-type
       :tag tag}))))

(declare gen-boxed)
(defmethod gen ::static-field
  [{:keys [target member java-type unbox-type position]}]
  `([:getstatic ~member]
    ~@(when (not= java-type unbox-type)
        (gen-boxed unbox-type))
    ~@(maybe-pop position java-type)))

(defmethod gen ::instance-field
  [{:keys [target target-class member field java-type unbox-type position]}]
  (if field
    `(~@(gen target)
      [:checkcast ~target-class]
      [:getfield ~field]
      ~@(when (not= java-type unbox-type)
          (gen-boxed unbox-type))
      ~@(maybe-pop position java-type))
    `(~@(gen target)
      [:ldc ~member]
      [:invokestatic ~[Reflector 'invokeNoArgInstanceMember
                       [:method Object [Object String]]]]
      ~@(maybe-pop position))))

;; TODO: fix boxing
(defmethod gen ::static-method
  [{:keys [target member method
           java-type unbox-type position args]}]
  (if method
    `(~@(mapcat gen-typed-arg (seq (.getParameterTypes method)) args)
      [:invokestatic ~method]
      ~@(maybe-pop position unbox-type))
    `([:ldc ~(.getName target)]
      [:invokestatic ~[Class 'forName [:method Class [String]]]]
      [:ldc ~member]
      ~@(gen-array args)
      [:invokestatic ~[Reflector 'invokeStaticMethod
                       [:method Object [Class String [:array Object]]]]]
      ~@(maybe-pop position))))

(defmethod gen ::instance-method
  [{:keys [target target-class member method
           java-type unbox-type position args]}]
  (if method
    `(~@(gen target)
      ~@(mapcat gen-typed-arg (seq (.getParameterTypes method)) args)
      [:invokevirtual ~method]
      ~@(maybe-pop position java-type))
    `(~@(gen target)
      [:ldc ~member]
      ~@(gen-array args)
      [:invokestatic ~[Reflector 'invokeInstanceMethod
                       [:method Object [Object String [:array Object]]]]]
      ~@(maybe-pop position))))

(defmethod eval-toplevel ::instance-method
  [{:keys [java-type target ^Method meth member args]} loader]
  (let [inst (eval-toplevel target loader)
        argvals (into-array (map #(eval-toplevel % loader) args))]
    (if meth
      (.invoke meth inst (box-args (.getParameterTypes meth) argvals))
      (Reflector/invokeInstanceMethod inst (str member) argvals))))

(defmethod analyze [::special '.]
  [[_ target member & args :as form]]
  (if (< (count form) 3)
    (throw (IllegalArgumentException.
             "Malformed member expression, expecting (. target member ...)"))
    (let [memb (if (seq? member) (first member) member)
          argl (if (seq? member) (next member) args)
          ^Class c (maybe-class *ns* target false)
          field? (and (= (count form) 3)
                      (or (symbol? member) (keyword? member)))]
      (when (or (not (symbol? memb))
                (and (seq? member) (seq args)))
        (throw (Exception. "Malformed member expression")))
      (run [pos (update-val :position #(if (= % :eval) :eval :expression))
            unboxed? (set-val :want-unboxed nil)
            inst (if c (m-result nil) (analyze target))
            field? (m-result
                     (if (and field? (not (keyword? memb)))
                       (if c
                         (empty? (Reflector/getMethods
                                   c 0 (munge-impl (name memb)) true))
                         (if (and inst (:java-type inst)
                                  (not (.isPrimitive (:java-type inst))))
                           (empty? (Reflector/getMethods
                                     (:java-type inst) 0
                                     (munge-impl (name memb)) false))
                           field?))
                       field?))
            _ (set-val :want-unboxed true)
            argl (m-map analyze argl)
            _ (set-val :position pos)]
        (assoc
          (if field?
            (analyze-field form c inst memb unboxed?)
            (analyze-java-method form c inst memb argl unboxed?))
          :position pos)))))

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
                 ;; TODO special case: instanceof
                 ;; TODO: keyword invoke, static invoke
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
                v (analyze `(~'var ~target))
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
  `(~@(gen target)
    ~@(when metamap
        `([:dup]
          ~@(gen metamap)
          [:checkcast ~IPersistentMap]
          [:invokevirtual ~[Var 'setMeta [:method :void [IPersistentMap]]]]))
    ~@(when init-provided?
        `([:dup]
          ~@(gen init)
          [:invokevirtual ~[Var 'bindRoot [:method :void [Object]]]]))
    ~@(maybe-pop position)))

(defmethod eval-toplevel ::def
  [{:keys [metamap target init init-provided?]} loader]
  (let [the-var (eval-toplevel target loader)]
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

;;;;

(defn- analyze-with-type [[form typ]]
  (let [f (analyze form)]
    (fn [ctx]
      (-> ctx
        (assoc :want-unboxed (if (is-primitive? typ) typ nil))
        f))))

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
               is (m-map analyze-with-type
                         (map vector inits (map :java-type loop-locals)))]
              is)]
        [{::etype ::recur
          :position position ; redundant
          :inits (doall inits)
          :loop-label loop-label
          :loop-locals loop-locals}         
         ctx]))))

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

(defmethod gen ::recur
  [{:keys [inits loop-label loop-locals]}]
  `(~@(mapcat (fn [i lb]
                (let [have-type (:java-type i)
                      need-type (:java-type lb)]
                  (cond
                    (= have-type need-type)
                    (gen i)
                    (is-primitive? need-type)
                    (concat
                      (gen i)
                      (coerce-primitive need-type))
                    :else
                    `(~@(gen i)
                      [:checkcast ~need-type]))))
              inits loop-locals)
    ~@(map (fn [lb] [(store-op (:java-type lb)) (:label lb)])
           (reverse loop-locals))
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
       :java-type (:java-type tail)
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
      [pos (fetch-val :position)
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

(defn gen-boxed [orig-type]
  (cond
    (= orig-type Boolean/TYPE)
    (let [[falsel endl] (map gensym ["False__ " "End__"])]
    `([:ifeq ~falsel]
      [:getstatic ~[Boolean 'TRUE Boolean]]
      [:goto ~endl]
      [:label ~falsel]
      [:getstatic ~[Boolean 'FALSE Boolean]]
      [:label ~endl]))
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
    `([:invokestatic ~[Float 'valueOf [:method Float [:float]]]]))
    (= orig-type Double/TYPE)
    `([:invokestatic ~[Double 'valueOf [:method Double [:double]]]])
    :else nil)

(defmethod gen ::if
  [{:keys [position java-type test then else]}]
  (let [lt (:java-type then) rt (:java-type else)
        coerce-left (if (= lt java-type)
                      (gen then)
                      (concat (gen then) (gen-boxed lt)))
        coerce-right (if (= rt java-type)
                       (gen else)
                       (concat (gen else) (gen-boxed rt)))]
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

;;;; Let & loop

(declare analyze-loop)

(defmethod analyze [::special 'let*]
  [form]
  (analyze-loop form false))

(defmethod analyze [::special 'loop*]
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
       _ (if loop?
           (set-val :loop-locals (map first bindings))
           (m-result nil))
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
       ~[RT 'vector [:method IPersistentVector [[:array Object]]]]])))

(defn emit-constants [obj cref mref]
  (doseq [{:keys [val java-type obj orig]} (:constants obj)]
    (asm/emit cref mref
      `(~@(gen-constant orig)
        [:checkcast ~java-type]
        [:putstatic [~obj ~val ~java-type]]))))

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
  (println "Compile class" (:name obj) "from" (:form obj))
  (asm/assembling [c {:name (:name obj)
                      :extends super
                      :implements ifaces
                      :flags #{:public :super :final}}]
    ;; static fields for constants
    (doseq [fld (:constants obj)]
      (let [{:keys [val java-type]} fld]
        (asm/add-field c {:name val
                          :descriptor java-type
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
          loader-code (m-map compile1 (form-seq rd))
          context (set-state null-context)]
         (let [clj-init (asm/add-method ns-init
                                        {:name 'load
                                         :flags #{:public :static}
                                         :descriptor [:method :void []]})]
           (asm/emit ns-init clj-init
                     (apply concat loader-code))
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
           (let [ns-init (asm/assemble-class ns-init)])))))))

(defn- compile1
  "Compile and eval a top-level form"
  [form]
  (run [form (macroexpand-impl *ns* form)
        res (if (and (seq? form) (= (first form) 'do))
              (m-map compile1 (next form))
              (run [ _ (m-result (println "Analyzing" form))
                    analyzed (analyze form)
                    loader (fetch-val :loader)]
                (let [bytecode (gen analyzed)]
                  (println "Eval-toplevel" form)
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
