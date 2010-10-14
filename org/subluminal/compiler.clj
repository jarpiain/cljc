(ns org.subluminal.compiler
  (:import (java.io Reader)
           (java.util IdentityHashMap)
           (clojure.lang LineNumberingPushbackReader
                         RT Var Symbol Keyword ISeq IFn))
  (:use (clojure.contrib monads))
  (:require (org.subluminal [class-file :as asm])))


(defstruct context
           :kind        ; :fn :method :loop :let :catch
           :position    ; :eval :statement :expression :return
           :lexicals    ; symbol -> struct lex-binding
           :loop-locals ; vector lex-binding
           :loop-label  ; symbol
           :method      ; method form
           :fn          ; struct fn-class
           :catching    ; catch-finally

           :clear-tag   ; gensym - identity
           :clear-root  ; context
           :clear-kind  ; :branch :path
           :clear-path  ; seq of context
           :binding-sites ; map of label -> lex-binding-inst
           :parent)     ; context

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
   (fn [ctx]
     (println "binding a" kind "in m-tag" (:method-tag (:method ctx)))
     (let [b {:symbol sym
              :label (gensym "BB")
              :kind kind
              :method-tag (:method-tag (:method ctx))
              :fn-tag (:fn-tag (:fn ctx))
              :java-type jtype
              :clear-root (:clear-root ctx)}]
       [b (update-in ctx [:lexicals]
                     assoc sym b)]))))

(defn clear-path [b]
  (let [p (reverse (next (take-while identity (iterate :clear-path b))))]
    (println "clear-path" (:symbol b)
             (map (juxt :clear-tag :clear-kind) p))
    p))

(defn join-point [b1 b2]
  (loop [p1 (clear-path b1) p2 (clear-path b2)]
    (cond
      (not= (:clear-tag (first p1)) (:clear-tag (first p2)))
      (do (println "early out") nil)

      (or (nil? (second p1)) (not= (:clear-tag (second p1))
                                   (:clear-tag(second p2))))
      (first p1)

      :else
      (recur (next p1) (next p2)))))

(defn make-binding-instance [b]
  (fn [ctx]
    (println "clear-root b=" (:clear-root b) "vs ctx=" (:clear-root ctx))
    (let [inst (assoc b :clear-path ctx
                        :live (atom (not= (:clear-root b)
                                          (:clear-root ctx))))
          lives (get-in ctx [:binding-sites (:label b)])
          lives (reduce (fn [coll inst2]
                          (let [j (join-point inst inst2)]
                            (println "join for" (:symbol b)
                                     "is" (:clear-kind j))
                            (if (not= (:clear-kind j) :branch)
                              (do (reset! (:live inst2) true) coll)
                              (conj coll inst2))))
                        []
                        lives)]
      [(dissoc inst :clear-path)
       (assoc-in ctx [:binding-sites (:label b)]
                 (conj lives inst))])))

(defn boxing-unify [x y]
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

(defn push-fn-context [f]
  (let [f (assoc f
                 :constants []
                 :constant-ids (IdentityHashMap.)
                 :keywords {}
                 :vars {}
                 :keyword-callsites {}
                 :var-callsites {}
                 :closed-lexicals {})]
    (fn [ctx]
      [nil (assoc ctx
                  :parent ctx
                  :fn f)])))

(defn push-method-context [m]
  (println "Setting loop-label->" (:loop-label m))
  (fn [ctx]
    [nil
     (assoc ctx
            :position :return
            :parent ctx
            :method (assoc m :method-tag (gensym "MT"))
;            :lexicals (merge (:lexicals ctx)
;                             (:params m))
            :loop-label (:loop-label m))]))
;            :loop-locals (:loop-locals m))]))

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

;(defn push-loop-context [inits]
;  (domonad state-m))

(defn pop-frame [ctx]
  [nil (assoc (:parent ctx)
              :binding-sites (:binding-sites ctx))])

(def null-context
  (let [t (gensym "NULL")]
    {:kind nil
     :position :eval
     :lexicals {}
     :loop-locals []
     :loop-label nil
     :method nil
     :fn nil
     :catching nil
     :clear-tag t
     :clear-root t
     :clear-kind :path
     :binding-sites {}
     :parent nil}))

(defn valid-binding? [b ctx]
  (println "Checking" (:kind b) "tagged"
           (:method-tag  b)
           "from" (:method-tag (:method ctx)))
  (case (:kind b)
    :closed (= (:fn-tag b) (:fn-tag (:fn ctx)))
    (:local :fnarg)
    (= (:method-tag b)
       (:method-tag (:method ctx)))))

(defn close-over [b]
  (fn updater [ctx]
    (if (valid-binding? b ctx)
      ctx
      (let [ctx (updater (:parent ctx))
            bb (get-in ctx [:lexicals (:symbol b)])
            clos (:closed-lexicals (:fn ctx))]
        (println "building a closure...")
        (-> ctx
          (update-in [:lexicals (:symbol b)]
                     assoc :kind :closed
                           :closed-idx (count clos))
;                           :method-tag (:method ctx)
          (update-in [:fn :closed-lexicals] conj bb))))))

(defn resolve-lexical [sym ctx]
  (if-let [b (get (:lexicals ctx) sym)]
    (let [ctx ((close-over b) ctx)
          b (get (:lexicals ctx) sym)]
      [b ctx])  
    [nil ctx]))

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

(defn etype [x]
  (if (nil? x)
    ::null
    (if-let [m (meta x)]
      (::etype m)
      (class x))))

(defn syncat [x]
  (cond
    (nil? x) ::null
    (or (true? x) (false? x)) ::boolean
    (symbol? x) ::symbol
    (string? x) ::string
    (keyword? x) ::keyword
    (and (coll? x) (empty? x)) ::empty
    (seq? x) [::special (first x)]
    (vector? x) ::vector
    (map? x) ::map
    :else ::constant))

(defmulti analyze syncat :default ::invocation)
#_(defmethod analyze :default
  [form]
  (fn [ctx]
    (throw (IllegalArgumentException.
             (str "Can't analyze " form)))))

(defmethod analyze ::null
  [form]
  (with-monad state-m
    (m-result nil)))

(defmethod analyze ::boolean
  [form]
  (with-monad state-m
    (m-result form)))

(defmethod analyze ::keyword
  [form]
  (with-monad state-m
    (m-result form)))

(defmethod analyze ::symbol
  [sym]
  (fn [ctx]
    (let [[lex ctx] (resolve-lexical sym ctx)]
      (if-not lex
        ['var ctx]
        ((make-binding-instance lex) ctx)))))

(defmethod analyze [::special 'monitor-enter]
  [[_ lockee]]
  (domonad state-m
    [pos (set-val :position :expression)
     lockee (analyze lockee)
     _ (set-val :position pos)]
    (with-meta `(~'monitor-enter ~lockee)
               {::etype ::monitor-enter})))

(defmethod analyze [::special 'monitor-exit]
  [[_ lockee]]
  (domonad state-m
    [pos (set-val :position :expression)
     lockee (analyze lockee)
     _ (set-val :position pos)]
    (with-meta `(~'monitor-exit ~lockee)
               {::etype ::monitor-exit})))

(defmethod analyze ::invocation
  [[op & args :as form]]
  (domonad state-m
    [pos (update-val :position #(if (= % :eval) :eval :expression))
     op (analyze op)
     ;; special case: instanceof
     ;; special case: keyword invoke, static invoke
     args (m-map analyze args)
     _ (set-val :position pos)]
    (with-meta `(~op ~@(doall args))
               {::etype ::invocation})))

(defmethod analyze [::special 'set!]
  [[_ target value :as form]]
  (if (not= (count form) 3)
    (throw (IllegalArgumentException.
             "Malformed assignment, expecting (set! target val)"))
    (domonad state-m
      [pos (set-val :position :expression)
       target (analyze target)
       value (analyze value)
       _ (set-val :position pos)]
      (if (isa? (etype target) ::assignable)
        (with-meta `(~'set! ~target ~value)
                   {::etype ::set!})
        (throw (IllegalArgumentException. "Invalid assignment target"))))))

(defmethod analyze [::special 'recur]
  [[_ & inits :as form]]
  (fn [{:keys [position loop-locals loop-label catching] :as ctx}]
    (println "position =" position "in recur")
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
            ((with-monad state-m (m-map analyze inits)) ctx)
            analyzed-form (list* 'recur inits)]
        [(with-meta analyzed-form
                    (merge (meta form)
                           {::etype ::recur
                            ::loop-label loop-label
                            ::loop-locals loop-locals}))
         ctx]))))

(defmethod analyze [::special 'do]
  [[_ & body]]
  (cond
    (empty? body)
    (analyze nil)

    (empty? (next body))
    (analyze (first body))

    :else
    (domonad state-m
      [pos (update-val :position #(if (= % :eval) :eval :statement))
       stmts (m-map analyze (butlast body))
       _ (set-val :position pos)
       tail (analyze (last body))]
      (with-meta `(~'do ~@(doall stmts) ~tail)
                 {::etype ::do}))))

(defmethod analyze [::special 'if]
  [[_ test-expr then else :as form]]
  (cond
    (> (count form) 4)
    (throw (Exception. "Too many arguments to if"))

    (< (count form) 3)
    (throw (Exception. "Too few arguments to if"))

    :else
    (domonad state-m
      [test-expr (analyze test-expr)
       _ (push-clear-node :branch false)
       _ (push-clear-node :path false)
       then (analyze then)
       _ pop-frame
       _ (push-clear-node :path false)
       else (analyze else)
       _ pop-frame
       _ pop-frame]
      (with-meta `(if ~test-expr ~then ~else)
                 (merge (meta form)
                        {::etype ::if})))))

(declare analyze-loop)

(defmethod analyze [::special 'let*]
  [form]
  (analyze-loop form false))

(defmethod analyze [::special 'loop]
  [form]
  (analyze-loop form true))

(defn analyze-init
  [[sym init]]
  (cond
    (not (symbol? sym))
    (throw (IllegalArgumentException.
             (str "Bad binding form, expected symbol, got: " sym)))

    (namespace sym)
    (throw (IllegalArgumentException.
             (str "Can't let qualified name: " sym)))

    :else
    (domonad state-m
      [init (analyze init)
       lb (make-binding sym nil)]
      ; maybe coerce init
      [lb init])))

(defn analyze-loop0
  [bindings body loop?]
  (domonad state-m
    [_ (push-frame)
     bindings (m-map analyze-init (partition 2 bindings))
     _ (set-val :loop-locals (map first bindings))
     _ (if loop? (set-val :position :return) (m-result nil))
     _ (if loop? (push-clear-node :path true) (m-result nil))
     body (analyze `(~'do ~@body))
     _ (if loop? pop-frame (m-result nil))
     _ pop-frame]
    (with-meta `(~'let* ~bindings ~body)
               {::etype (if loop? ::loop ::let)})))

(defn analyze-loop
  [[_ bindings & body :as form] loop?]
  (cond
    (not (vector? bindings))
    (throw (IllegalArgumentException. "Bad binding form, expected vector"))

    (odd? (count bindings))
    (throw (IllegalArgumentException.
             "Bad binding form, expected matching symbol expression pairs"))

    :else
    (domonad state-m
      [pos (fetch-val :position)
       r (if (or (= pos :eval)
                 (and loop? (= pos :expression)))
           (analyze `(fn* [] ~form))
           (analyze-loop0 bindings body loop?))]
      r)))

(defn normalize-fn*
  [[op & opts :as form]]
  (let [this-name (when (symbol? (first opts)) (first opts))
        static? (:static (meta this-name))]
    (domonad state-m
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

(declare analyze-method)

(def +max-positional-arity+ 20)
(defn arity [m]
  (count (:required-params (:argv m))))

(defmethod analyze [::special 'fn*]
  [[op & opts :as form]]
  (domonad state-m
    [[op & meth :as norm] (normalize-fn* form)
     _ (push-fn-context (meta norm))
     meth (m-map analyze-method meth)
     f (fetch-val :fn)
     _ pop-frame]
    (loop [a (vec (repeat (inc +max-positional-arity+) nil))
           variadic nil
           remain meth]
      (if (seq remain)
        (let [[info body :as m] (first remain)
              ar (arity info)]
          (println "Arity" ar "method")
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
              (when (some identity (subvec a v-ar))
                (throw (Exception.
                         (str "Can't have fixed arity function"
                              " with more params than variadic function"))))))
          (when (and (:static? f)
                     (not (empty? (:closed-lexicals f))))
            (throw (Exception. "Static fns can't be closures")))
          ;; compile & load
          ;; add metadata from original form
          (assoc f
                 :methods (filter identity a)
                 :variadic-method variadic))))))


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
          (let [c (tag-class (tag-of arg))
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

(defn variadic? [argv]
  (not (nil? (:rest-param argv))))

(defn analyze-method
  [[argv & body]]
  (println "Analyzing" argv body)
  (if (not (vector? argv))
    (throw (IllegalArgumentException.
             "Malformed method, expected argument vector"))
    (let [ret-class (tag-class (tag-of argv))]
      (when (and (.isPrimitive ret-class)
                 (not (or (= ret-class Long/TYPE) (= ret-class Double/TYPE))))
        (throw (IllegalArgumentException.
                 "Only long and double primitives are supported")))
      (domonad state-m
        [{:keys [this-name static?] :as this-fn} (fetch-val :fn)
         argv (m-result (process-fn*-args argv static?))
         _ (push-method-context {:loop-label (gensym "LL")
                                 :loop-locals nil})
         _ (push-clear-node :path true)
         thisb (if static?
                 (m-result nil)
                 (make-binding (or this-name (gensym "THIS")) IFn :fnarg))
         bind (m-map (fn [[s t]] (make-binding s t :fnarg))
                     (if (variadic? argv)
                       (conj (:required-params argv)
                             (:rest-param argv))
                       (:required-params argv)))
         _ (update-val :method #(assoc % :loop-locals bind))
         _ (set-val :loop-locals bind)
         body (analyze `(~'do ~@body))
         m (fetch-val :method)
         _ pop-frame
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

;; methods for

;;;; Analyze
;; tags a form with ::etype
;; (maybe also ::can-emit-primitive? ::java-class)

(declare analyze-seq analyze-symbol analyze-fn)
(defmulti analyze-special first)

(defn analyze
  ([ctx form] (analyze ctx form nil))
  ([ctx form name]
   (let [form (if (instance? LazySeq form)
                (or (seq form) ())
                form)
         cls (class form)]
     (cond
       (or (true? form) (false? form) (nil? form)) form
       (= cls Symbol) (analyze-symbol ctx form)
       (= cls Keyword) (register-keyword form)
       (isa? cls Number) form
       (= cls String) form

       (and (coll? form) (empty? form))
       (with-meta form {::etype ::empty})

       (seq? form)
       (analyze-seq ctx form name)

       (vector? form)
       (with-meta form {::etype ::vector})

       (map? form)
       (with-meta form {::etype ::map})

       (set? form)
       (with-meta form {::etype ::set})

       :else
       (with-meta [form] {::etype ::constant})))))


(defmulti gen (fn [form ctx] (etype form)))
(defmulti literal-val etype)

(defmethod literal-val :default
  [form]
  (throw (IllegalArgumentException. (str "Not a literal form: " form))))

(defmethod analyze-special :default
  [form]
  (throw (IllegalArgumentException. (str "Unrecognized special form " form))))

(defmethod gen :default
  [form _ _]
  (throw (IllegalArgumentException. (str "Can't eval " form))))

;; clojure.core/import* deftype* new reify*
(def +specials+
  #{'def 'loop 'recur 'if 'let* 'letfn*
    'do 'fn* 'quote 'var 'import* '.
    'set! 'try 'throw 'monitor-enter
    'monitor-exit 'catch 'finally
    'new})

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

;;;; macro expansion

(def *macroexpand-limit* 100)
(declare macroexpand1-impl)

(defn macroexpand-impl [form]
  (loop [f form n 0]
    (let [fx (macroexpand1-impl f)]
      (if (identical? f fx)
        f
        (if (and *macroexpand-limit* (>= n *macroexpand-limit*))
          (throw (RuntimeException. (str "Runaway macroexpansion: " form)))
          (recur fx (inc n)))))))

(defn macroexpand1-impl [form]
  (if-not (seq? form)
    form
    (let [op (first form)]
      (if (contains? +specials+ op)
        form
        (if-let [v (the-macro op)]
          (apply v form *local-env* (next form))
          (if-not (symbol? op)
            form
            (let [sname (name op)]
              (cond
                (= (.charAt sname 0) \.)
                (if-not (next form) ; (< length 2)
                  (throw (IllegalArgumentException.
                           (str "Malformed member expression: " form
                                ", expecting (.member target ...)")))
                  (let [meth (symbol (.substring sname 1))
                        target (second form)
                        target (if nil ; (maybe-class target false)
                                 (with-meta
                                   `(identity ~target)
                                   {:tag Class})
                                 target)]
                    (with-meta
                      `(~'. ~target ~meth ~@(nnext form))
                      (meta form))))

                (names-static-member? op)
                (let [target (symbol (namespace op))
                      meth (symbol (name op))
                      c String] ;(maybe-class target false)]
                  (if-not c form
                    (with-meta
                      `(~'. ~target ~meth ~@(next form))
                      (meta form))))

                (.endsWith sname ".")
                (with-meta
                  `(~'new ~(symbol (.substring sname (dec (count sname)))) ~@(next form))
                  (meta form))

                :else form))))))))

;;;; bytecode generation (gen unboxed-gen)

;;;; nil literal

(defmethod literal-val ::null
  [form]
  nil)

(defmethod gen ::null
  [form ctx]
  `([:aconst-null]
    ~@(when (= ctx :statement) [[:pop]])))

;;;; boolean literal

(defmethod literal-val Boolean
  [form]
  form)

(defmethod gen Boolean
  [form ctx]
  `(~(if form
       [:getstatic [Boolean 'TRUE Boolean]]
       [:getstatic [Boolean 'FALSE Boolean]])
    ~@(when (= ctx :statement)
        [[:pop]])))

;;;; String literal

(defmethod literal-val String
  [form]
  form)

(defmethod gen String
  [form ctx]
  `([:ldc ~form]
    ~@(when (= ctx :statement)
        [[:pop]])))

;;;; if

(defn gen-if
  [[test then else] ctx unboxed?]
  (let [[nulll falsel endl] (repeatedly 3 gensym)]
    `(~@(if (= (maybe-primitive-type test) :boolean)
          `(~@(gen-unboxed test :expression)
            [:ifeq ~falsel])
          `(~@(gen test :expression)
            [:dup]
            [:ifnull ~nulll]
            [:getstatic [Boolean 'FALSE Boolean]]
            [:if-acmpeq ~falsel]))
      ~@(if unboxed?
          (gen-unboxed then ctx)
          (gen then ctx))
      [:goto ~endl]
      [:label ~nulll]
      [:pop]
      [:label ~falsel]
      ~@(if unboxed?
          (gen-unboxed else ctx)
          (gen else ctx))
      [:label ~endl])))

(defmethod gen ::if
  [form ctx]
  (gen-if form ctx false))

(defmethod gen-unboxed ::if
  [form ctx]
  (gen-if form ctx true))

;;;; do

(defn gen-body
  [forms ctx unboxed?]
  (lazy-seq
    (when (seq forms)
      (if (next forms)
        (concat (gen (first forms) :statement)
                (gen-body (next forms) ctx))
        (if unboxed?
          (gen-unboxed (first forms) ctx)
          (gen (first forms) ctx))))))

(defn gen ::do
  [[_ & body] ctx]
  (gen-body body ctx false))

(defn gen-unboxed ::do
  [[_ & body] ctx]
  (gen-body body ctx true))

;;;; toplevel

;; A constant is
;; ^{::etype ::constant} {:type foo :val v}

(defn compile
  [rd src-path src-name]
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
        bytecode))))
