(in-ns 'org.subluminal.compiler)

;;;; Expressions that require analyzing subforms

;;;; If

(defn common-superclass [^Class l ^Class r]
  {:pre [(and l r (not (.isPrimitive l)) (not (.isPrimitive r)))]}
  (cond
    (isa? l r) r
    (isa? r l) l
    ;; Interfaces report nil superclass
    :else (recur
            (or (.getSuperclass l) Object)
            (or (.getSuperclass r) Object))))

(defn common-supertype [^Class l ^Class r]
  (cond
    (= l r) l
    ;(= l Bottom) r
    ;(= r Bottom) l
    (valid-promotion? l r) r
    (valid-promotion? r l) l
    (or (prim-type? l) (prim-type? r))
    (common-supertype (boxed-version l l) (boxed-version r r))
    ;; Both are ref types at this point
    (nil? l) r
    (nil? r) l
    ;; Both are non-null ref types
    :else (common-superclass l r)))

(defmethod analyze [::special 'if]
  [pos typ _ [_ tst then else :as form]]
  (cond
    (> (count form) 4)
    (throw (Exception. "Too many arguments to if"))
    
    (< (count form) 3)
    (throw (Exception. "Too few arguments to if"))

    :else
    (run [tst (analyze :expression Boolean/TYPE nil tst)
          _ (push-clear-node :branch false)
          _ (push-clear-node :path false)
          then (analyze pos typ nil then)
          _ pop-frame
          _ (push-clear-node :path false)
          else (analyze pos typ nil else)
          _ pop-frame
          _ pop-frame]
      (let [left (:gen-type then)
            right (:gen-type else)
            gen 
            (if (= left right)
                  left
                  (common-supertype left right))]
        {::etype ::if
         :gen-type gen ; will be Void/TYPE if (= pos :statement)
         :tst tst
         :then then
         :else else}))))

(defmethod gen ::if
  [{:keys [gen-type tst then else]}]
  (if (= (:gen-type tst) Boolean/TYPE)
    (let [[elsel endl] (map gensym ["Else__" "Endif__"])]
      `(~@(gen tst)
        [:ifeq ~elsel]
        ~@(gen then)
        ~@(gen-convert (:gen-type then) gen-type)
        ;~@(when (not= (:gen-type then) Bottom)
        ;    (list [:goto endl])) ; dead code avoidance
        [:goto ~endl]
        [:label ~elsel]
        ~@(gen else)
        ~@(gen-convert (:gen-type else) gen-type)
        [:label ~endl]))
    (let [[falsel null endl] (map gensym ["False__" "Null__" "Endif__"])]
      `(~@(gen tst)
        [:dup]
        [:ifnull ~null]
        [:getstatic ~[Boolean 'FALSE Boolean]]
        [:if-acmpeq ~falsel]
        ~@(gen then)
        ~@(gen-convert (:gen-type then) gen-type)
        ;~@(when (not= (:gen-type then) Bottom)
        ;    (list [:goto endl]))
        [:goto ~endl]
        [:label ~null]
        [:pop]
        [:label ~falsel]
        ~@(gen else)
        ~@(gen-convert (:gen-type else) gen-type)
        [:label ~endl]))))

(defmethod eval-toplevel ::if
  [{:keys [tst then else]} loader]
  (if (eval-toplevel tst loader)
    (eval-toplevel then loader)
    (eval-toplevel else loader)))

;;;; Do

(defmethod analyze [::special 'do]
  [pos typ _ [_ & body]]
  (cond
    (empty? body)
    (analyze pos typ nil nil)

    (empty? (next body))
    (analyze pos typ nil (first body))

    :else
    (run [stmts (m-map (partial analyze :statement Void/TYPE nil) (butlast body))
          tail (analyze pos typ nil (last body))]
      {::etype ::do
       :gen-type (:gen-type tail)
       :body (doall (concat stmts [tail]))})))

(defmethod gen ::do
  [{:keys [body]}]
  (mapcat gen body))

(defmethod eval-toplevel ::do
  [{:keys [body]} loader]
  (dorun (map #(eval-toplevel % loader) (butlast body)))
  (eval-toplevel (last body) loader))

;;;; Let / loop

;; In this form, the inner if statement is analyzed in
;; :return contest with void type request.
;; Checking against pos = :statement when determining
;; if gen-type should be void resulted in a stack height
;; mismatch --> will check against typ = void instead
(comment
(def nasty-form '(fn [x]
                   (cond
                     true
                     (do
                       (loop [] (if true (recur) nil))
                       x))
                   nil)))

;; kind is :local :fnarg :closed
;; label is used only for the 'this' local of fn methods
(defn make-binding
  [sym stype gtype kind lbl]
  {:pre [(gen-contract stype gtype)]}
  (fn [ctx]
   (let [b {:symbol sym
            :label (or lbl (gensym "LB__"))
            :force-live (when lbl true) ; don't clear 'this'
            :kind kind
            :fn-tag (:fn ctx)
            :gen-type gtype
            :source-type stype
            :clear-root (:clear-root ctx)}]
     [b (update-in ctx [:lexicals]
                   assoc sym b)])))

(defn- local-type
  "Decide the type of a let-local in case
  there is both a type hint and inferred type
  for the initializer expression"
  [loop? hinted inferred]
  (cond
    (nil? hinted)
    (if (or (prim-type? inferred)
            (not loop?))
      inferred
      Object)

    (prim-type? inferred)
    (if hinted
      (throw (Exception. "Can't type hint a local with a primitive initializer"))
      inferred)

    :else hinted))

(defn analyze-init
  [loop? [sym init]]
  (let [hint (when (:tag (meta sym)) (tag-class *ns* (tag-of sym)))]
    (cond
      (not (symbol? sym))
      (throw (IllegalArgumentException.
               "Bad binding form, expected symbol, got: " sym))

      (namespace sym)
      (throw (IllegalArgumentException.
               (str "Can't let qualifield name: " sym)))

      :else
      (run [init (analyze :expression true nil init)
            jtype (m-result (local-type loop? hint (:gen-type init)))
            lb (make-binding sym jtype jtype :local nil)]
        [lb init]))))

(defn analyze-loop
  [pos typ [_ bindings & body :as form] loop?]
  (cond
    (= pos :eval)
    (analyze pos typ nil `((~'fn* [] ~form)))

    (not (vector? bindings))
    (throw (IllegalArgumentException. "Bad binding form, expected vector"))

    (odd? (count bindings))
    (throw (IllegalArgumentException.
             "Bad binding form, expected matching symbol expression pairs"))

    :else
    (let [loop-label (gensym "Loop__")]
      (run [_ (push-frame) ; local bindings
            bindings (m-map (partial analyze-init loop?)
                            (partition 2 bindings))
            _ (if loop?
                (set-vals :loop-label loop-label
                          :loop-locals (map first bindings))
                (m-result nil))
            _ (if loop? (push-clear-node :path true) (m-result nil))
            body (analyze (if loop? :return pos)
                          typ nil `(~'do ~@body))
            _ (if loop? pop-frame (m-result nil))
            _ pop-frame] ; local bindings
        {::etype ::loop
         :gen-type (if (= typ Void/TYPE) typ (:gen-type body))
         :bindings bindings
         :loop-label loop-label
         :body body}))))

(defmethod analyze [::special 'let*]
  [pos typ _ form]
  (analyze-loop pos typ form false))

(defmethod analyze [::special 'loop*]
  [pos typ _ form]
  (analyze-loop pos typ form true))

(defmethod gen ::loop
  [{:keys [gen-type bindings loop-label body]}]
  (let [lbs (map first bindings)
        block-init (mapcat (juxt :label :gen-type) lbs)]
    `((:block nil nil ~block-init
        ~@(mapcat
            (fn [[lb init]]
              `(~@(gen init)
                ~@(gen-convert (:gen-type init) (:source-type lb))
                ~[(store-op (:source-type lb)) (:label lb)]))
            bindings)
        [:label ~loop-label]
        ~@(gen body)))))

;;;; Recur

(defn- analyze-with-type [[form typ]]
  (analyze :expression typ nil form))

(defmethod analyze [::special 'recur]
  [pos typ _ [_ & inits :as form]]
  (fn [{:keys [loop-locals loop-label catching] :as ctx}]
    (cond
      (or (not= pos :return) (nil? loop-label))
      (throw (Exception. "Can only recur from tail position"))

      catching
      (throw (Exception. "Cannot recur from catch/finally"))

      (not (== (count inits) (count loop-locals)))
      (throw (Exception.
        (format "Mismatched argument count to recur, expected: %d args, got %d"
                (count loop-locals)
                (count inits))))

      :else
      (run-with ctx
        [inits (m-map analyze-with-type
                      (map vector inits (map :gen-type loop-locals)))]
        {::etype ::recur
         :gen-type nil
         :inits (doall inits)
         :loop-label loop-label
         :loop-locals loop-locals}))))

(defmethod gen ::recur
  [{:keys [inits gen-type loop-label loop-locals]}]
  `(~@(mapcat (fn [init lb]
                (let [source (:gen-type init)
                      target (:source-type lb)]
                  `(~@(gen init)
                    ~@(gen-coerce source target))))
              inits loop-locals)
    ~@(map (fn [lb] [(store-op (:source-type lb)) (:label lb)])
           (reverse loop-locals))
    [:goto ~loop-label]))

;;;; Throw

(defmethod analyze [::special 'throw]
  [pos typ _ [_ ex :as form]]
  (if (= pos :eval)
    (analyze pos typ nil `((~'fn* [] ~form)))
    (run [ex (analyze :expression nil nil ex)]
      {::etype ::throw
       :gen-type (if (= typ Void/TYPE) Void/TYPE nil)
       :ex ex})))

(defmethod gen ::throw
  [{:keys [ex]}]
  `(~@(gen ex)
    ~@(gen-convert (:gen-type ex) Throwable)
    [:athrow]))

;;;; Try/catch/finally

(defn- catch-clause? [form]
  (and (seq? form)
       (contains? #{'catch 'finally} (first form))))

(defn analyze-catch-clause
  [pos typ [op & args]]
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
                 (str "Can't bind qualified name: " evar)))
        :else
        (run [_ (push-frame)
              lb (make-binding evar eclass eclass :local nil)
              _ (set-val :catching true)
              body (analyze pos typ nil `(~'do ~@body))
              _ pop-frame]
          {::etype ::catch
           :gen-type (:gen-type body)
           :bind lb
           :body body})))
    'finally
    (run [_ (push-frame)
          _ (set-val :catching true)
          body (analyze :statement Void/TYPE nil `(~'do ~@args))
          _ pop-frame]
      {::etype ::finally
       :body body})))

(defmethod analyze [::special 'try]
  [pos typ _ [_ & body :as form]]
  (if (not= pos :return)
    (analyze pos typ _ `((~'fn* [] ~form)))
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
      (run [body (analyze pos typ nil `(~'do ~@body))
            clauses (m-map (partial analyze-catch-clause
                                    pos (:gen-type body))
                           clauses)]
        (let [final (last clauses)
              final (when (= (::etype final) ::finally) final)]
          {::etype ::try
           :gen-type (:gen-type body)
           :body body
           :clauses (if final (butlast clauses) clauses)
           :final final})))))

(defn gen-catch
  [{:keys [gen-type final]}
   [start-try end-try _ retl finallyl ret-local]
   {:keys [bind body]}]
  (let [eclass (:gen-type bind)
        hstart (gensym "Catch__")
        hend (gensym "Endcatch__")]
    `((:block ~hstart ~hend ~[(:label bind) (:gen-type bind)]
        [:astore ~(:label bind)]
        ~@(gen body)
        ~@(gen-coerce (:gen-type body) gen-type)
        ~@(when (not= gen-type Void/TYPE)
            (list [(store-op gen-type) ret-local])))
      ~@(when final
          (gen (:body final)))
      [:goto ~retl]
      [:catch ~start-try ~end-try ~hstart ~eclass]
      ~@(when final (list [:catch hstart hend finallyl nil])))))

(defmethod gen ::try
  [{:keys [gen-type body clauses final] :as whole}]
  (let [[start-try end-try endl retl finallyl ret-local :as labels]
        (map gensym ["Try__" "End_Try__" "End__" "Ret__" "Fin__" "Retvar__"])]
    `((:block ~start-try ~endl ~[ret-local (or gen-type Object)]
        ~@(gen body)
        ~@(when (not= gen-type Void/TYPE)
            (list [(store-op gen-type) ret-local]))
        [:label ~end-try]
        ~@(when final
            (gen (:body final)))
        [:goto ~retl]
        ~@(mapcat (partial gen-catch whole labels) clauses)
        ~@(when final
            (let [final-ex (gensym "Fex__")]
              `((:block ~finallyl nil [~final-ex ~Throwable]
                  [:astore ~final-ex]
                  ~@(gen (:body final))
                  [:aload ~final-ex]
                  [:athrow])
                [:catch ~start-try ~end-try ~finallyl nil])))
        [:label ~retl]
        ~@(when (not= gen-type Void/TYPE)
            (list [(load-op gen-type) ret-local]))))))

;;;; Fn invocation

(defmethod analyze ::invocation
  [pos typ name [op & args :as form]]
  (let [tag (tag-class *ns* (tag-of form))]
    (run [me (macroexpand-impl *ns* form)
          res (if-not (identical? me form)
                (analyze pos typ name me)
                (run [op (analyze :expression nil nil op)
                      args (m-map (partial analyze :expression true nil) args)]
                  {::etype ::invocation
                   :op op
                   :args (doall args)
                   :gen-type (cond
                               (= typ Void/TYPE)
                               Void/TYPE
                               (and tag (analyze-contract typ tag)
                                    (gen-contract Object tag))
                               tag
                               :else Object)}))]
      res)))

(defmethod gen ::invocation
  [{:keys [op args gen-type]}]
  (let [[posargs varargs] (split-at 20 args)]
    `(~@(gen op)
      ~@(gen-convert (:gen-type op) IFn)
      ~@(mapcat (fn [arg]
                  `(~@(gen arg)
                    ~@(gen-convert (:gen-type arg) Object)))
                posargs)
      ~@(when (seq varargs) (gen-array varargs))
      [:invokeinterface
       ~[IFn 'invoke [:method Object (if (seq varargs)
                                       (concat (repeat 20 Object)
                                               [[:array Object]])
                                       (repeat (count args) Object))]]]
      ~@(gen-convert Object gen-type))))

(defmethod eval-toplevel ::invocation
  [{:keys [op args]} loader]
  (let [eop (eval-toplevel op loader)
        eargs (doall (map #(eval-toplevel % loader) args))]
    (apply eop eargs)))

;;;; Member access expressions

(derive ::static-field ::assignable)
(derive ::static-method ::assignable)

(defn member-type
  "Determine the type of a member expression
  when a :tag hint and a request for a primitive
  type may be given"
  [source req hint]
  {:post [(or % (= source Void/TYPE))]}
  (cond
    (= req Void/TYPE) req
    (and hint (analyze-contract req hint)
         (gen-contract source hint))
    hint
    (analyze-contract req source)
    source
    (gen-contract source req)
    (if req req (boxed-version source))
    :else (boxed-version source source)))

(defn analyze-field [form ^Class c inst member type-req]
  {:post [(analyze-contract type-req (:gen-type %))]}
  (let [sym (if (symbol? member)
              member
              (symbol (name member)))
        tag (tag-class *ns* (tag-of form))]
    (if c
      (let [^Field fld (.getField c (munge-impl (name sym)))
            source-type (.getType fld)
            gen-type (member-type source-type type-req tag)]
        {::etype ::static-field
         :target-class c
         :member fld
         :gen-type gen-type
         :source-type source-type})
      (let [^Class c (if (:gen-type inst)
                       (:gen-type inst)
                       Object)
            ^Field fld (Reflector/getField
                         c (munge-impl (name sym)) false)
            source-type (if fld (.getType fld) Object)
            gen-type (member-type source-type type-req tag)]
        {::etype ::instance-field
         :target-class c
         :target inst
         :member fld
         :member-name (munge-impl (name sym))
         :gen-type gen-type
         :source-type source-type}))))

(defn analyze-method-call
  [form ^Class c inst member args type-req]
  (let [sym (if (symbol? member)
              member
              (symbol (name member)))
        tag (tag-class *ns* (tag-of form))]
    (if c
      (let [methods (Reflector/getMethods c (count args) (name sym) true)
            ^Method the-method
            (cond
              (empty? methods)
              (throw (Exception. (str "No matching method: " member)))
              (== (count methods) 1)
              (first methods)
              :else
              (matching-method (map :gen-type args) methods))
            source-type (if the-method (.getReturnType the-method) Object)
            gen-type (member-type source-type type-req tag)]
        {::etype ::static-method
         :target-class c
         :args args
         :member the-method
         :member-name (munge-impl (name sym))
         :source-type source-type
         :gen-type gen-type})
      (let [c (or (:gen-type inst) Object)
            methods (Reflector/getMethods c (count args) (name sym) false)
            ^Method the-method
            (cond
              (empty? methods) nil
              (== (count methods) 1)
              (first methods)
              :else
              (matching-method (map :gen-type args) methods))
            source-type (if the-method (.getReturnType the-method) Object)
            gen-type (member-type source-type type-req tag)]
        {::etype ::instance-method
         :target inst
         :target-class c
         :args args
         :member the-method
         :member-name (munge-impl (name sym))
         :gen-type gen-type
         :source-type source-type}))))

(defmethod analyze [::special '.]
  [pos typ _ [_ target member & args :as form]]
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
      (run [inst (if c (m-result nil) (analyze :expression nil nil target))
            argl (m-map (partial analyze :expression true nil) argl)]           
        (let [field? (when (and field? (not (keyword? memb)))
                       (if c
                         (empty? (Reflector/getMethods
                                   c 0 (munge-impl (name memb)) true))
                         (empty? (Reflector/getMethods
                                   (or (:gen-type inst) Object) 0
                                   (munge-impl (name memb)) false))))]
          (if field?
            (analyze-field form c inst memb typ)
            (analyze-method-call form c inst memb argl typ)))))))

(defmethod gen ::static-field
  [{:keys [target source-type gen-type member]}]
  `([:getstatic ~member]
    ~@(gen-convert source-type gen-type)))

(defmethod gen ::instance-field
  [{:keys [target target-class member member-name source-type gen-type]}]
  (if member
    `(~@(gen target)
      [:getfield ~member]
      ~@(gen-convert source-type gen-type))
    ;; Need reflection
    `(~@(gen target)
      [:ldc ~member-name]
      [:invokestatic ~[Reflector 'invokeNoArgInstanceMember
                       [:method Object [Object String]]]]
      ~@(gen-convert source-type gen-type))))

(defmethod eval-toplevel ::static-field
  [{:keys [^Field member]} loader]
  (.get member nil))

(defmethod eval-toplevel ::instance-field
  [{:keys [target target-class ^Field member member-name]} loader]
  (let [inst (eval-toplevel target loader)]
    (if member
      (.get member inst)
      (Reflector/invokeNoArgInstanceMember inst member-name))))

(defmethod gen ::static-method
  [{:keys [target-class member member-name gen-type source-type args]}]
  (if member
    `(~@(mapcat (fn [typ arg]
                  `(~@(gen arg)
                    ~@(gen-coerce (:gen-type arg) typ)))
                (seq (.getParameterTypes member))
                args)
      [:invokestatic ~member]
      ~@(gen-convert source-type gen-type))
    ;; Need reflection
    `([:ldc ~(.getName target-class)]
      [:invokestatic ~[Class 'forName [:method Class [String]]]]
      [:ldc ~member-name]
      ~@(gen-array args)
      [:invokestatic ~[Reflector 'invokeStaticMethod
                       [:method Object [Class String [:array Object]]]]]
      ~@(gen-convert Object gen-type))))

(defmethod gen ::instance-method
  [{:keys [target ^Method member member-name gen-type source-type args]}]
  (if member
    `(~@(gen target)
      ~@(mapcat (fn [typ arg]
                  `(~@(gen arg)
                    ~@(gen-coerce (:gen-type arg) typ)))
                (seq (.getParameterTypes member))
                args)
      ~(if (.isInterface (.getDeclaringClass member))
         [:invokeinterface member]
         [:invokevirtual member])
      ~@(gen-convert source-type gen-type))
    ;; Need reflection
    `(~@(gen target)
      [:ldc ~member-name]
      ~@(gen-array args)
      [:invokestatic ~[Reflector 'invokeInstanceMethod
                       [:method Object [Object String [:array Object]]]]]
      ~@(gen-convert Object gen-type))))

(defmethod eval-toplevel ::static-method
  [{:keys [target-class member member-name args]} loader]
  (let [argvals (map #(eval-toplevel % loader) args)]
    (if member
      (.invoke member nil
               (box-args (.getParameterTypes member) argvals))
      (Reflector/invokeStaticMethod target-class member-name
                                    (into-array Object argvals)))))

(defmethod eval-toplevel ::instance-method
  [{:keys [target member member-name args]} loader]
  (let [inst (eval-toplevel target loader)
        argvals (map #(eval-toplevel % loader) args)]
    (if member
      (.invoke member inst
               (box-args (.getParameterTypes member) argvals))
      (Reflector/invokeInstanceMethod inst member-name
                                      (into-array Object argvals)))))

;;;; New

(defmethod analyze [::special 'new]
  [pos typ _ [_ cls & ctor-args :as form]]
  (if (< (count form) 2)
    (throw (Exception. (str "Wrong number of arguments, "
                            "expecting (new Classname args...)")))
    (let [^Class c (maybe-class *ns* cls false)
          tag (tag-class *ns* (tag-of form))]
      (if-not c
        (throw (IllegalArgumentException.
                 (str "Unable to resolve classname: " cls)))
        (run [args (m-map (partial analyze :expression true nil) ctor-args)]
          (let [ctors (filter (fn [^Constructor ctor]
                                (== (count (.getParameterTypes ctor))
                                    (count args)))
                              (.getConstructors c))
                match
                (cond
                  (empty? ctors)
                  (throw (Exception. (str "No matching ctor found for " c)))
                  (empty? (next ctors))
                  (first ctors)
                  :else
                  (matching-constructor (map :gen-type args) ctors))
                gen-type (member-type c typ tag)]
            {::etype ::new
             :gen-type gen-type
             :source-type c
             :ctor match
             :args args}))))))

(defmethod gen ::new
  [{:keys [^Class source-type ^Class gen-type ^Constructor ctor args]}]
  (if ctor
    `([:new ~source-type]
      [:dup]
      ~@(mapcat (fn [typ arg]
                  `(~@(gen arg)
                    ~@(gen-coerce (:gen-type arg) typ)))
                (seq (.getParameterTypes ctor)) args)
      [:invokespecial ~ctor]
      ~@(gen-convert source-type gen-type))
    `([:ldc ~(.getName source-type)]
      [:invokestatic ~[Class 'forName [:method Class [String]]]]
      ~@(gen-array args)
      [:invokestatic ~[Reflector 'invokeConstructor
                       [:method Object [Class [:array Object]]]]]
      ~@(gen-convert Object gen-type))))

(defmethod eval-toplevel ::new
  [{:keys [^Constructor ctor ^Class source-type args]} loader]
  (let [argvals (map #(eval-toplevel % loader) args)]
    (if ctor
      (.newInstance ctor (box-args (.getParameterTypes ctor) argvals))
      (Reflector/invokeConstructor source-type
                                   (into-array Object argvals)))))

;;;; Collection literals

(defmethod analyze ::vector
  [pos typ _ vect]
  (run [comps (m-map (partial analyze :expression nil nil) vect)
        res (if (every? #(isa? (::etype %) ::constant) comps)
              (register-constant typ (vec (map :lit-val comps)))
              (m-result {::etype ::vector
                         :components comps
                         :gen-type (if (= typ Void/TYPE)
                                     typ
                                     IPersistentVector)}))]
    ;; TODO metadata
    res))

(defmethod analyze ::set
  [pos typ _ elts]
  (run [elts (m-map (partial analyze :expression nil nil) elts)
        res (if (every? #(isa? (::etype %) ::constant) elts)
              (register-constant typ (set (map :lit-val elts)))
              (m-result {::etype ::set
                         :elements elts
                         :gen-type (if (= typ Void/TYPE)
                                     typ
                                     IPersistentSet)}))]
    res))

(defmethod analyze ::map
  [pos typ _ entries]
  (run [entries (m-map (partial analyze :expression nil nil)
                       (apply concat (seq entries)))
        res (if (every? #(isa? (::etype %) ::constant) entries)
              (register-constant typ (apply array-map (map :lit-val entries)))
              (m-result {::etype ::map
                         :entries entries
                         :gen-type (if (= typ Void/TYPE)
                                     typ
                                     IPersistentMap)}))]
    res))

(defmethod gen ::vector
  [{:keys [components gen-type]}]
  `(~@(gen-array components)
    [:invokestatic
     ~[RT 'vector [:method IPersistentVector [[:array Object]]]]]
    ~@(gen-convert IPersistentVector gen-type)))

(defmethod gen ::set
  [{:keys [elements gen-type]}]
  `(~@(gen-array elements)
    [:invokestatic
     ~[RT 'set [:method IPersistentSet [[:array Object]]]]]
    ~@(gen-convert IPersistentSet gen-type)))

(defmethod gen ::map
  [{:keys [entries gen-type]}]
  `(~@(gen-array entries)
    [:invokestatic
     ~[RT 'map [:method IPersistentMap [[:array Object]]]]]
    ~@(gen-convert IPersistentMap gen-type)))

(defmethod eval-toplevel ::vector
  [{:keys [components]} loader]
  (into [] (map #(eval-toplevel % loader) components)))

(defmethod eval-toplevel ::set
  [{:keys [elements]} loader]
  (into #{} (map #(eval-toplevel % loader) elements)))

(defmethod eval-toplevel ::map
  [{:keys [entries]} loader]
  (apply array-map (map #(eval-toplevel % loader) entries)))

;;;; Synchronization

(defmethod analyze [::special 'monitor-enter]
  [pos typ _ [_ lockee]]
  (run [lockee (analyze :expression nil nil lockee)]
    {::etype ::monitor-enter
     :lock lockee
     :gen-type
     (if (= typ Void/TYPE) typ nil)}))

(defmethod analyze [::special 'monitor-exit]
  [pos typ _ [_ lockee]]
  (run [lockee (analyze :expression nil nil lockee)]
    {::etype ::monitor-exit
     :lock lockee
     :gen-type
     (if (= typ Void/TYPE) typ nil)}))

(defmethod gen ::monitor-enter
  [{:keys [lock gen-type]}]
  `(~@(gen lock)
    [:monitorenter]
    ~@(gen-convert Void/TYPE gen-type)))

(defmethod gen ::monitor-exit
  [{:keys [lock gen-type]}]
  `(~@(gen lock)
    [:monitorexit]
    ~@(gen-convert Void/TYPE gen-type)))

(defmethod eval-toplevel ::monitor-enter
  [{:keys [lock]} loader]
  (monitor-enter (eval-toplevel lock loader)))

(defmethod eval-toplevel ::monitor-exit
  [{:keys [lock]} loader]
  (monitor-exit (eval-toplevel lock loader)))

;;;; Def

(defmethod analyze [::special 'def]
  [pos typ _ [_ target init :as form]]
  (cond
    (> (count form) 3)
    (throw (Exception. "Too many arguments to def"))
    (< (count form) 2)
    (throw (Exception. "Too few arguments to def"))
    (not (symbol? target))
    (throw (Exception. "First argument to def must be a Symbol"))
    :else
    ;; compile-time intern
    (let [^Var v (lookup-var *ns* target true)]
      (if (nil? v)
        (throw (Exception. "Can't refer to qualified var that doesn't exist"))
        (let [v (cond
                  (= (.ns v) *ns*) v
                  (nil? (namespace target)) (intern *ns* target)
                  :else (throw (Exception.
                                 "Can't create defs outside of current ns")))
              mm (meta target)]
          (run [meta-expr (if mm
                            (analyze (if (= pos :eval) pos :expression)
                                     nil nil mm)
                            (m-result nil))
                init-expr (analyze (if (= pos :eval) pos :expression)
                                   nil (name target) init)]
            {::etype ::def
             :metamap meta-expr
             :target v
             :gen-type (if (= typ Void/TYPE) typ Var)
             :init init-expr
             :init-provided? (== (count form) 3)}))))))

(declare gen-constant)
(defmethod gen ::def
  [{:keys [metamap target init init-provided? gen-type]}]
  `(~@(gen-constant target)
    ~@(when metamap
        `([:dup]
          ~@(gen metamap)
          [:checkcast ~IPersistentMap]
          [:invokevirtual ~[Var 'setMeta [:method :void [IPersistentMap]]]]))
    ~@(when init-provided?
        `([:dup]
          ~@(gen init)
          [:invokevirtual ~[Var 'bindRoot [:method :void [Object]]]]))
    ~@(gen-convert Var gen-type)))

(defmethod eval-toplevel ::def
  [{:keys [metamap target init init-provided?]} loader]
  (let [the-var target]
    (if init-provided?
      (.bindRoot the-var (eval-toplevel init loader))
      (if metamap
        (reset-meta! the-var (eval-toplevel metamap loader))))
    the-var))
