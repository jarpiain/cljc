(in-ns 'org.subluminal.compiler)

(defn namespace-for
  ([sym] (namespace-for *ns* sym))
  ([inns sym]
   (let [nssym (symbol (namespace sym))
         n (or (get (ns-aliases inns) nssym)
               (find-ns nssym))]
     n)))

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

;; Try to resolve (in order):
;; 1. fully.qualified.Class or [LArrayClass;
;;    -> Class object or ClassNotFoundException
;; 2. always resolve the symbols 'ns' and 'in-ns' in namespace clojure.core
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
          {::etype ::static-field
           :class c
           :name (symbol (name sym))}
          (throw (Exception. (str "Unable to find static field: " (name sym)
                                  " in " c))))
        (throw (Exception. (str "No such namespace: " ns-part)))))))

(defn resolve-sym [n sym]
  (if (namespace sym)
    (resolve-qualified n sym)
    (resolve-unqualified n sym)))

;; used in (analyze ::symbol)
(defn lookup-sym [n sym]
  (let [o (resolve-sym n sym)]
    (cond
      (var? o)
      (if (.isMacro o)
        (throw (Exception. "Can't take value of a macro: " o))
        (register-var o))
      (class? o)
      (register-constant o)
      :else
      (m-result {::etype ::static-field}))))

;; used in (def sym ...) and (var sym)
(defn lookup-var [rel-ns sym intern?]
  (cond
    (namespace sym)
    (let [ns-part (symbol (namespace sym))
          name-part (symbol (name sym))
          ns-for (or (.lookupAlias rel-ns ns-part)
                     (Namespace/find ns-part))]
      (println "ns-for" ns-for)
      (when ns-for
        (if (and intern? (= ns-for rel-ns))
          (intern ns-for name-part))
          (.findInternedVar ns-for name-part)))
    (= sym 'ns)
    #'clojure.core/ns
    (= sym 'in-ns)
    #'clojure.core/in-ns
    :else
    (let [o (.getMapping rel-ns sym)]
      (if o
        (if (instance? Var o)
          o
          (throw (Exception. (str "Expecting var, but sym: " sym
                                  " is mapped to " o))))
        (when intern?
          (intern rel-ns sym))))))

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

(defn munge-impl
  "Convert the name of a Clojure symbol into a valid Java identifier"
  [s]
  (apply str (map #(get +char-map+ % %) s)))

;;;; Resolve :tag metadata to a class

(defn maybe-class
  [^Namespace nss tag string-ok?]
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
        (let [c (.getMapping nss tag)]
          (if (class? c) c
            (RT/classForName (name tag))))
        (catch ClassNotFoundException e
          nil)))

    (and string-ok? (string? tag))
    (RT/classForName ^String tag)))

(def ^{:private true} prim-class
  {'int Integer/TYPE
   'long Long/TYPE
   'double Double/TYPE
   'float Float/TYPE
   'char Character/TYPE
   'short Short/TYPE
   'byte Byte/TYPE
   'boolean Boolean/TYPE
   'void Void/TYPE })

(def ^{:private true} array-tags
   {'objects (class (make-array Object 0))
    'ints (class (int-array 0))
    'longs (class (long-array 0))
    'floats (class (float-array 0))
    'doubles (class (double-array 0))
    'bytes (class (byte-array 0))
    'shorts (class (short-array 0))
    'chars (class (char-array 0))
    'booleans (class (boolean-array 0))})

(defn- tag-to-class [nss tag]
  (if-let [c (array-tags tag)] c
    (if-let [c (maybe-class nss tag true)] c
      (throw (IllegalArgumentException.
               (str "Unable to resolve classname: " tag))))))

(defn tag-class
  "Try to interpret the :tag metadata of an object as a Class"
  [nss tag]
  (if (nil? tag)
    Object
    (if-let [c (prim-class tag)]
      c
      (tag-to-class nss tag))))

(defn tag-of
  "Returns the :tag metadata of an object as a symbol"
  [thing]
  (let [tag (:tag (meta thing))]
    (cond
      (symbol? tag)
      tag
      (string? tag)
      (symbol tag)
      :else nil)))
