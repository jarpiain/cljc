(in-ns 'org.subluminal.compiler)

;;;; Expressions that correspond to java classes

(declare compile-class)

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

;;;; Fn

(defn normalize-fn*
  [pos this-name [op & opts :as form]]
  (let [this-name (if (symbol? (first opts))
                    (first opts)
                    this-name)
        opts (if (symbol? (first opts))
               (next opts)
               opts)
        methods (if (vector? (first opts))
                  (list opts)
                  opts)]
    (run [enc current-object]
      (let [_ (println "This-name" this-name "enc" (:name enc))
            base-name (if enc
                        (str (:name enc) "$")
                        (str (munge-impl (name (ns-name *ns*))) "$"))
            simple-name (if this-name
                          (str (.replace (munge-impl (name this-name))
                                         "." "_DOT_")
                               (if enc (str "__" (RT/nextID)) ""))
                          (str "fn__" (RT/nextID)))]
        (with-meta `(~'fn* ~@methods)
                   {:src form
                    :this-name this-name ; symbol used for self-recursion
                    :enclosing-method enc
                    :once-only (:once (meta op))
                    :fn-tag (gensym "FN")
                    ;; name of generated class
                    :name (symbol (str base-name simple-name))})))))

(defn process-fn*-args [argv]
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
          (let [c (or (tag-class *ns* (tag-of arg)) Object)
                c (if (= state :rest) ISeq c)]
            (cond
              (.isPrimitive c)
              (throw (Exception.
                (str "Fn can't have primitive parameter: " arg)))

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
  (println "Analyze-method" body)
  (if (not (vector? argv))
    (throw (IllegalArgumentException.
             "Malformed method, expected argument vector"))
    (let [ret-class (or (tag-class *ns* (tag-of argv)) Object)
          argv (process-fn*-args argv)]
      (when (.isPrimitive ret-class)
        (throw (IllegalArgumentException.
                 "Primitive return type is not supported")))
      #_(when (and (.isPrimitive ret-class)
                 (not (or (= ret-class Long/TYPE) (= ret-class Double/TYPE))))
        (throw (IllegalArgumentException.
                 "Only long and double primitives are supported")))
      (run [{:keys [this-name] :as this-fn} current-object
            _ (push-method-frame {:loop-label (gensym "LL")
                                  :loop-locals nil})
            thisb (make-this-binding
                    (or this-name (gensym "THIS")) IFn)
            ;; FIXME: rest arg should have ISeq source type
            bind (m-map (fn [[sym tag]] (make-binding sym Object tag
                                                      :fnarg nil))
                        (if (variadic? argv)
                          (conj (:required-params argv)
                                (:rest-param argv))
                          (:required-params argv)))
            _ (update-current-method assoc :loop-locals bind)
            _ (set-val :loop-locals bind)
            body (analyze :return nil nil `(~'do ~@body))
            m current-method
            _ pop-frame]
        (assoc m
               :body body
               :argv argv
               :bind bind
               :this thisb)))))

(def max-positional-arity 20)
(def variadic-index (inc max-positional-arity))

(defn- variadic? [argv]
  (not (nil? (:rest-param argv))))

(defn- arity [m]
  (count (:required-params (:argv m))))

(defn- update-method-array [arr {:keys [argv] :as m}]
  (let [n (arity m)]
    (if (variadic? argv)
      (if (arr variadic-index)
        (throw (Exception. "Can't have more than 1 variadic overload"))
        (assoc arr variadic-index m))
      (if (arr n)
        (throw (Exception. "Can't have 2 overloads with same arity"))
        (assoc arr n m)))))

;; the assembler recognizes 'this as a special label
(defn make-this-binding
  [sym jtype]
  (make-binding sym jtype jtype :fnarg 'this))

(defmethod analyze [::special 'fn*]
  [pos typ name [op & opts :as form]]
  (run [classloader (fetch-val :loader)
        [op & meth :as norm] (normalize-fn* pos name form)
        _ (push-object-frame (meta norm))
        meth (m-reduce update-method-array
                       (vec (repeat (+ 2 max-positional-arity) nil))
                       (doall (map analyze-method meth)))
        f current-object
        _ pop-frame
        initargs (m-map (partial analyze :expression true nil)
                        (map :symbol (vals (:closed-lexicals f))))]
    (let [[variadic-info _] (meth variadic-index)
          variadic-arity (when variadic-info (arity variadic-info))]
      (when (and variadic-info
                 (some identity (subvec meth (inc variadic-arity)
                                        variadic-index)))
        (throw (Exception.
                 (str "Can't have fixed arity function"
                      " with more params than variadic function"))))
      ;; TODO: add metadata from original form
      (let [obj (assoc f :methods (filter identity meth)
                         :form form
                         :variadic-arity variadic-arity)]
        (compile-class classloader obj (if variadic-info RestFn AFunction) [])
        {::etype ::fn
         :class-name (:name obj)
         :gen-type (if (= typ Void/TYPE) typ IFn)
         :initargs initargs}))))

(defmethod gen ::fn
  [{:keys [class-name initargs gen-type]}]
  `([:new ~class-name]
    [:dup]
    ~@(apply concat
        (for [b initargs]
          (gen b)))
    [:invokespecial ~[class-name '<init>
                      [:method :void (for [init initargs]
                                       (:source-type init))]]]
    ~@(gen-convert IFn gen-type)))

(defmethod eval-toplevel ::fn
  [{:keys [class-name initargs]} loader]
  (assert (empty? initargs))
  (let [^Class c (.loadClass loader (str class-name))
        ^Constructor ctor
        (.getConstructor c (make-array Class 0))]
    (.newInstance ctor (make-array Object 0))))
