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

#_(defn close-over [binding method]
  (when (and binding method
             (not (contains? (:locals method) binding)))
    (dosync
      (alter method update-in [:closes] assoc binding binding))
    (close-over binding (:parent method))))

#_(defn reference-local [sym]
  (if-not (bound? #'*local-env*)
    nil
    (let [b (get *local-env* sym)]
      (when b
        (close-over *method* b))
      b)))

#_(defn the-macro [sym]
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

(def +char-map+
  {\- "_"
   \: "_COLON_"
   \+ "_PLUS_"
   \> "_GT_"
   \< "_LT_"
   \= "_EQ_"
   \~ "_TILDE_"
   \! "_BANG_"
   \@ "_CIRCA_"
   \# "_SHARP_"
   \$ "_DOLLARSIGN_"
   \% "_PERCENT_"
   \^ "_CARET_"
   \& "_AMPERSAND_"
   \* "_STAR_"
   \| "_BAR_"
   \{ "_LBRACE_"
   \} "_RBRACE_"
   \[ "_LBRACK_"
   \] "_RBRACK_"
   \/ "_SLASH_"
   \\ "_BSLASH_"
   \? "_QMARK_"})

(defn munge-impl [s]
  (apply str (map #(get +char-map+ % %) s)))

(defn maybe-class
  [tag string-ok?]
  (cond
    (class? tag) tag

    (and (symbol? tag) (not (namespace tag)))
    (cond
;      (= tag *compile-stub-sym*)
;      *compile-stub-class*

      (or (> (.indexOf (name tag) (int \.)) 0)
          (= (.charAt (name tag) 0) (int \[)))
      (RT/classForName (name tag))

      :else
      (try
        (let [c (resolve tag)]
          (if (class? c) c
            (RT/classForName (name tag))))
        (catch ClassNotFoundException e
          nil)))

    (and string-ok? (string? tag))
    (RT/classForName ^String tag)))

(def prim-class
  {'int Integer/TYPE
   'long Long/TYPE
   'double Double/TYPE
   'float Float/TYPE
   'char Character/TYPE
   'short Short/TYPE
   'byte Byte/TYPE
   'boolean Boolean/TYPE
   'void Void/TYPE })

(def array-tags
   {'objects (class (make-array Object 0))
    'ints (class (int-array 0))
    'longs (class (long-array 0))
    'floats (class (float-array 0))
    'doubles (class (double-array 0))
    'bytes (class (byte-array 0))
    'shorts (class (short-array 0))
    'chars (class (char-array 0))
    'booleans (class (boolean-array 0))})

(defn tag-to-class [tag]
  (if-let [c (array-tags tag)] c
    (if-let [c (maybe-class tag true)] c
      (throw (IllegalArgumentException.
               (str "Unable to resolve classname: " tag))))))

(defn tag-class [tag]
  (if (nil? tag)
    Object
    (if-let [c (prim-class tag)]
      c
      (tag-to-class tag))))

(defn tag-of [thing]
  (let [tag (:tag (meta thing))]
    (cond
      (symbol? tag)
      tag
      (string? tag)
      (symbol tag)
      :else nil)))
