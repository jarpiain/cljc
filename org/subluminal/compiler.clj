(ns org.subluminal.compiler
  (:import (java.io Reader)
           (clojure.lang LineNumberingPushbackReader
                         RT Var Symbol Keyword))
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
           :method ; relevant for :local :fnarg
           :closed-idx
           :java-type
           :fn
           :clear-root
           ;; for binding instances
           :live
           :clear-path)

(defn make-binding [sym jtype]
  (fn [ctx]
    (let [b {:symbol sym
             :label (gensym)
             :kind :local
             :method (:method ctx)
             :java-type jtype
             :clear-root (:clear-root ctx)}]
      [b (update-in ctx [:lexicals]
                    assoc sym b)])))

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
  (fn [ctx]
    [nil (assoc ctx
                :parent ctx
                :fn f)]))

(defn push-method-context [m]
  (fn [ctx]
    [nil
     (assoc ctx
            :parent ctx
            :method m
            :lexicals (merge (:lexicals ctx)
                             (:method-params m))
            :loop-label (:loop-label m)
            :loop-locals (:loop-locals m))]))

(defn push-clear-node [kind root?]
  (fn [ctx]
    (let [tag (gensym)]
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
  (let [t (gensym)]
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
  (case (:kind b)
    :closed (= (:fn b) (:fn ctx))
    (:local :fnarg)
    (= (:method b) (:method ctx))))

(defn close-over [b]
  (fn updater [ctx]
    (if (valid-binding? b ctx)
      ctx
      (let [ctx (updater (:parent ctx))
            bb (get-in ctx [:lexicals (:symbol b)])
            clos (:closed-lexicals (:fn ctx))]
        (-> ctx
          (update-in [:lexicals (:symbol b)]
                     assoc :kind :closed
                           :closed-idx (count clos)
                           :method (:method ctx))
          (update-in [:fn :closed-lexicals] conj bb))))))

(defn resolve-lexical [sym ctx]
  (if-let [b (get (:lexicals ctx) sym)]
    (let [ctx ((close-over b) ctx)
          b (get (:lexicals ctx) sym)]
      [b ctx])  
    [nil ctx]))

(defn unify-loop [types]
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

(def *breaker* 0)

(defn syncat [x]
  (if (> *breaker* 100) (throw (Exception. "Broken")))
  (set! *breaker* (inc *breaker*))
  (println "Analyzing --> " x)
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

(defmulti analyze syncat)
(defmethod analyze :default
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
            ((m-map analyze inits) ctx)
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
      [pos (set-val :position :statement)
       stmts (m-map analyze (butlast body))
       _ (set-val :position pos)
       tail (analyze (last body))]
      (with-meta `(~'do ~@stmts ~tail)
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

;; context values
;; :statement :expression :return :eval


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
