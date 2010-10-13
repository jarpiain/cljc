(in-ns 'org.subluminal.compiler)

(load "cljc/util")

(defn namespace-for
  ([sym] (namespace-for *ns* sym))
  ([inns sym]
   (let [nssym (symbol (namespace sym))
         ns (or (get (ns-aliases inns) nssym)
                (find-ns nssym))]
     ns)))

(defn- lookup-var0 [sym intern?]
  (cond
    (namespace sym)
    (when-let [symns (namespace-for sym)]
      (let [name-part (symbol (name sym))]
        (if (and intern? (= symns *ns*))
          (intern *ns* name-part)
          (.findInternedVar symns name-part))))

    (= sym 'ns)
    #'clojure.core/ns

    (= sym 'in-ns)
    #'clojure.core/in-ns

    :else
    (let [o (.getMapping *ns* sym)]
      (cond
        (nil? o)
        (when intern?
          (intern *ns* (symbol (name sym))))

        (var? o)
        o

        :else
        (throw (Exception. (str "Expecting var, but " sym
                                " is mapped to " o)))))))

(defn lookup-var [sym intern?]
  (let [v (lookup-var0 sym intern?)]
;    (when v (register-var v))
    v))

(defn close-over [binding method]
  (when (and binding method
             (not (contains? (:locals method) binding)))
    (dosync
      (alter method update-in [:closes] assoc binding binding))
    (close-over binding (:parent method))))

(defn reference-local [sym]
  (if-not (bound? #'*local-env*)
    nil
    (let [b (get *local-env* sym)]
      (when b
        (close-over *method* b))
      b)))

(defn the-macro [sym]
  (cond
    ;; local macros...
    (and (symbol? sym) (reference-local sym))
    nil

    (or (symbol? sym) (var? sym))
    (let [^Var v (if (var? sym)
                   sym
                   (lookup-var sym false))]
      (when (and v (.isMacro v))
        (if (or (= (.ns v) *ns*)
                (.isPublic v))
          v
          (throw (IllegalStateException.
                   (str "var: " v " is not public"))))))))

(defn names-static-member? [^Symbol sym]
  (and (.getNamespace sym)
       (not (namespace-for sym))))

(defn file->class-name [^String f]
  (symbol (str (-> f
                 (.replace java.io.File/separator ".")
                 (.substring 0 (.lastIndexOf f (int \.))))
               RT/LOADER_SUFFIX)))

(defn constant-name [n]
  (symbol (str "const__" n)))

(defn site-name [n]
  (symbol (str "__site__" n)))

(defn site-name-static [n]
  (symbol (str (site-name [n]) "__")))

(defn thunk-name [n]
  (symbol (str "__thunk__" n)))

(defn thunk-name-static [n]
  (symbol (str (thunk-name n) "__")))

(defn cached-class-name [n]
  (symbol (str "__cached_class__" n)))

(defn cached-proto-fn-name [n]
  (symbol (str "__cached_proto_fn__" n)))

(defn cached-proto-impl-name [n]
  (symbol (str "__cached_proto_impl__" n)))

(defn var-callsite-name [n]
  (symbol (str "__var__callsite__" n)))

;; common method specifiers
(def mspec
  {'kwintern  [Keyword "intern"   [:method Keyword [String String]]]
   'symcreate [Symbol  "create"   [:method Symbol [String]]]
   'varintern [Var     "intern"   [:method Var [String String]]]
   'getclass  [Class   "getClass" [:method Class []]]
   'get-class-loader [Class "getClassLoader" [:method ClassLoader []]]})
