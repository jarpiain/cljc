(in-ns 'org.subluminal.compiler)

(defn namespace-for
  ([sym] (namespace-for *ns* sym))
  ([inns sym]
   (let [nssym (symbol (namespace sym))
         n (or (get (ns-aliases inns) nssym)
               (find-ns nssym))]
     n)))

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
           :class c
           :fld f}
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

(defn the-macro [nss sym env]
  (cond
    ;; local macros...
    (and (symbol? sym) (contains? env sym))
    nil

    (or (symbol? sym) (var? sym))
    (let [^Var v (if (var? sym)
                   sym
                   (lookup-var nss sym false))]
      (when (and v (.isMacro v))
        (if (or (= (.ns v) nss)
                (.isPublic v))
          v
          (throw (IllegalStateException.
                   (str "var: " v " is not public"))))))))

(defn names-static-member? [nss ^Symbol sym]
  (and (namespace sym)
       (not (namespace-for nss sym))))

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

;; Macro expansion.
;; macroexpand exists in clojure.core
;; but just delegates to clojure.lang.Compiler

(def +specials+
  #{'if 'let* 'loop* 'fn* 'recur 'quote
    'var 'def 'monitor-enter 'monitor-exit
    'throw 'try 'catch 'finally 'new
    ;; TODO:
    'clojure.core/import* 'case* '.
    'letfn* 'set! 'deftype* 'reify* '&})

(defn macroexpand1-impl
  [nss env form]
  (if-not (seq? form)
    form
    (let [op (first form)]
      (if (contains? +specials+ op)
        form
        (if-let [v (the-macro nss op env)]
          (apply v form env (next form))
          (if-not (symbol? op)
            form
            (let [^String sname (name op)]
              ;; For some reason accepts symbols with arbitrary ns part
              (cond
                (= (.charAt sname 0) \.)
                (if-not (next form)
                  (throw (IllegalArgumentException.
                           (str "Malformed member expression: " form
                                ", expecting (.member target ...)")))
                  (let [member (symbol (.substring sname 1))
                        target (second form)
                        target (if (maybe-class nss target false)
                                 (with-meta
                                   `(identity ~target)
                                   {:tag Class})
                                 target)]
                    (with-meta
                      `(~'. ~target ~member ~@(nnext form))
                      (meta form))))

                (names-static-member? nss op)
                (let [target (symbol (namespace op))
                      member (symbol (name op))
                      c (maybe-class nss target false)]
                  (if-not c form
                    (with-meta
                      `(~'. ~target ~member ~@(next form))
                      (meta form))))

                (.endsWith sname ".")
                (with-meta
                  `(~'new ~(symbol (.substring sname 0 (dec (count sname))))
                       ~@(next form))
                  (meta form))

                :else form))))))))

(def *macroexpand-limit* 100)

(defn macroexpand-impl
  [nss form]
  (run [env (fetch-val :lexicals)]
    (loop [f form n 0]
      (let [fx (macroexpand1-impl nss env f)]
        (if (identical? f fx)
          f
          (if (and *macroexpand-limit* (>= n *macroexpand-limit*))
            (throw (Exception. (str "Runaway macroexpansion: " form)))
            (recur fx (inc n))))))))

;;;; Arg type matching

;; Why is Reflector/boxArgs not public?
(defn box-args [cls avals]
  (amap avals i _
        (let [^Class aclass (aget cls i)
              aval (aget avals i)]
          (cond
            (not (.isPrimitive aclass))
            (.cast aclass aval)
            (= aclass Boolean/TYPE)
            (.cast Boolean/TYPE aval)
            (= aclass Character/TYPE)
            (.cast Character/TYPE aval)
            :else
            (if (number? aval)
              (cond
                (= aclass Byte/TYPE)
                (.byteValue ^Number aval)
                (= aclass Short/TYPE)
                (.shortValue ^Number aval)
                (= aclass Integer/TYPE)
                (.intValue ^Number aval)
                (= aclass Long/TYPE)
                (.longValue ^Number aval)
                (= aclass Float/TYPE)
                (.floatValue ^Number aval)
                (= aclass Double/TYPE)
                (.doubleValue ^Number aval))
              (throw (IllegalArgumentException.
                       (str "Unexpected param type, expected "
                            aclass ", given: " (.getName (class aval))))))))))

(defn subsumes [^"[Ljava.lang.Class;" left
                ^"[Ljava.lang.Class;" right]
  (areduce left i ret false
           (let [l (aget left i) r (aget right i)]
             (cond
               (nil? ret) nil
               (= l r) ret
               (or (and (not (.isPrimitive l))
                        (.isPrimitive r))
                   (.isAssignableFrom r l))
               true
               :else nil))))

(defn matching-constructor
  [args ctors]
  (let [args (vec args)]
    (loop [best nil tied? false found-exact? false ctors ctors]
      (if (nil? ctors)
        (if tied?
          (throw (IllegalArgumentException.
                   "More than one matching constructor found"))
          best)
        (let [curr (first ctors)
              curr-types (.getParameterTypes curr)
              [exacts match?]
              (areduce 
                curr-types i r [0 true]
                (let [[exact match] r
                      parm (aget curr-types i)
                      arg (args i)]
                  (if (= parm arg)
                    [(inc exact) match]
                    [exact (Reflector/paramArgTypeMatch parm arg)])))]
          (cond
            (== exacts (count args))
            (recur
              (if (not found-exact?) curr best)
              tied? true (next ctors))
            (and match? (not found-exact?))
            (cond
              (nil? best)
              (recur curr tied? found-exact? (next ctors))
              (subsumes curr-types
                        (.getParameterTypes best))
              (recur curr false found-exact? (next ctors))
              (Arrays/equals curr-types
                             (.getParameterTypes best))
              (recur curr tied? found-exact? (next ctors))
              (not (subsumes (.getParameterTypes best)
                             curr-types))
              (recur best true found-exact? (next ctors))
              :else
              (recur best tied? found-exact? (next ctors)))
            :else (recur best tied? found-exact? (next ctors))))))))
