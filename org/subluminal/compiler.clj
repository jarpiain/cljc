(ns org.subluminal.compiler
  (:import (java.io Reader)
           (clojure.lang LineNumberingPushbackReader))
  (:require (org.subluminal [class-file :as asm])))

;;;; Util

(def *local-env* *method*)

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

(defn macro? [sym]
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

(defn names-static-member [^Symbol sym]
  (and (.getNamespace sym)
       (not (namespace-for sym))))

;; context values
;; :statement :expression :return :eval

(defn etype [x]
  (if (nil? x)
    ::null
    (if-let [m (meta x)]
      (::etype m)
      (class x))))

;; methods for
;; Symbol Keyword Number String

(declare analyze-seq analyze-symbol analyze-fn)

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

(defmulti analyze-special first)
(defmulti emit (fn [form ctx] (etype form)))
(defmulti literal-val etype)

(defmethod literal-val :default
  [form]
  (throw (IllegalArgumentException. (str "Not a literal form: " form))))

(defmethod analyze-special :default
  [form]
  (throw (IllegalArgumentException. (str "Unrecognized special form " form))))

(defmethod emit :default
  [form _ _]
  (throw (IllegalArgumentException. (str "Can't eval " form))))

;; clojure.core/import* deftype* new reify*
(def +specials+
  #{'def 'loop 'recur 'if 'let* 'letfn*
    'do 'fn* 'quote 'var 'import* '.
    'set! 'try 'throw 'monitor-enter
    'monitor-exit 'catch 'finally
    'new})

(derive ::if ::maybe-primitive)
(derive ::null ::literal)
(derive Boolean ::literal)
(derive Boolean ::primitive)
(derive ::primitive ::maybe-primitive)

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

;;;; nil literal

(defmethod literal-val ::null
  [form]
  nil)

(defmethod emit ::null
  [form ctx]
  (template
    ([:aconst-null]
     ~@(when (= ctx :statement) [[:pop]]))))

;;;; boolean literal

(defmethod literal-val Boolean
  [form]
  form)

(defmethod emit Boolean
  [form ctx]
  (template
    ~(if form
       [:getstatic [Boolean 'TRUE Boolean]]
       [:getstatic [Boolean 'FALSE Boolean]])
    ~@(when (= ctx :statement)
        [[:pop]])))

;;;; String literal

(defmethod literal-val String
  [form]
  form)

(defmethod emit String
  [form ctx]
  (template
    ([:ldc ~form]
     ~@(when (= ctx :statement)
         [[:pop]]))))

;;;; if

(defn emit-if
  [[test then else] ctx unboxed?]
  (let [[nulll falsel endl] (repeatedly 3 gensym)]
    (template
      (~@(if (= (maybe-primitive-type test) :boolean)
           (template
             (~@(emit-unboxed test :expression)
              [:ifeq ~falsel]))
           (template
             (~@(emit test :expression)
              [:dup]
              [:ifnull ~nulll]
              [:getstatic [Boolean 'FALSE Boolean]]
              [:if-acmpeq ~falsel])))
       ~@(if unboxed?
           (emit-unboxed then ctx)
           (emit then ctx))
       [:goto ~endl]
       [label ~nulll]
       [:pop]
       [label ~falsel]
       ~@(if unboxed?
           (emit-unboxed else ctx)
           (emit else ctx))
       [label ~endl]))))

(defmethod emit ::if
  [form ctx]
  (emit-if form ctx false))

(defmethod emit-unboxed ::if
  [form ctx]
  (emit-if form ctx true))
