;; Copyright (c) Juha Arpiainen. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'org.subluminal.compiler)

;;;; Manipulating lexical context

(defn current-method [ctx]
  [(get (:index ctx) (:method ctx)) ctx])

(defn update-current-method [f & args]
  (fn [ctx]
    [nil (apply update-in ctx [:index (:method ctx)] f args)]))

(defn current-object [ctx]
  [(get (:index ctx) (:fn ctx)) ctx])

(defn push-frame []
  (fn [ctx]
    [nil (assoc ctx :parent ctx)]))

(defn pop-frame [ctx]
  [nil (assoc (:parent ctx)
              :binding-sites (:binding-sites ctx)
              :index (:index ctx))])

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

;;;; Namespace lookup

(defn namespace-for
  ([sym] (namespace-for *ns* sym))
  ([inns sym]
   (let [nssym (symbol (namespace sym))
         n (or (get (ns-aliases inns) nssym)
               (find-ns nssym))]
     n)))

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
    nil
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

;;;; Macro expansion
;; macroexpand exists in clojure.core
;; but just delegates to clojure.lang.Compiler

(def +specials+
  #{'if 'let* 'loop* 'fn* 'recur 'quote
    'var 'def 'monitor-enter 'monitor-exit
    'throw 'try 'catch 'finally 'new '.
    'clojure.core/import*
    ;; TODO:
    'case* 'letfn* 'set! 'deftype* 'reify* '&})

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
                                   {:tag 'java.lang.Class})
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

(defn subsumes [^"[Ljava.lang.Class;" left
                ^"[Ljava.lang.Class;" right]
  (areduce left i ret false
           (let [^Class l (aget left i) ^Class r (aget right i)]
             (cond
               (nil? ret) nil
               (= l r) ret
               (or (and (not (.isPrimitive l))
                        (.isPrimitive r))
                   (.isAssignableFrom r l))
               true
               :else nil))))

;; TODO: define a protocol for Methods and Constructors
;; and merge these two fns:

(defn matching-method
  [args ctors]
  (let [args (vec args)]
    (loop [^Method best nil tied? false found-exact? false ctors ctors]
      (if (nil? ctors)
        (if tied?
          (throw (IllegalArgumentException.
                   "More than one matching method found"))
          best)
        (let [^Method curr (first ctors)
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
              (if (or (not found-exact?)
                      (not best)
                      (.isAssignableFrom (.getReturnType best)
                                         (.getReturnType curr)))
                curr best)
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
              (recur
                (if (.isAssignableFrom (.getReturnType best)
                                       (.getReturnType curr))
                  curr best)
                tied? found-exact? (next ctors))
              (not (subsumes (.getParameterTypes best)
                             curr-types))
              (recur best true found-exact? (next ctors))
              :else
              (recur best tied? found-exact? (next ctors)))
            :else (recur best tied? found-exact? (next ctors))))))))

(defn matching-constructor
  [args ctors]
  (let [args (vec args)]
    (loop [^Constructor best nil tied? false found-exact? false ctors ctors]
      (if (nil? ctors)
        (if tied?
          (throw (IllegalArgumentException.
                   "More than one matching constructor found"))
          best)
        (let [^Constructor curr (first ctors)
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
