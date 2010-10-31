(ns org.subluminal.reflect
  (:import (java.lang.reflect Field Method Constructor)
           (java.util Arrays)
           (clojure.lang Reflector)))

(defprotocol MethodSpecifier
  (return-type [x])
  (arg-types [x])
  (owner [x])
  (identifier [x]))

(extend-protocol MethodSpecifier
  Method 
    (return-type [x] (.getReturnType x))
    (arg-types [x] (.getParameterTypes x))
    (owner [x] (.getDeclaringClass x))
    (identifier [x] (symbol (.getName x)))
  Constructor
    (return-type [x] Void/TYPE)
    (arg-types [x] (.getParameterTypes x))
    (owner [x] (.getDeclaringClass x))
    (identifier [x] '<init>))

;; only used via MethodSpecifier methods
(deftype MethodInfo [ret args owner id]
  MethodSpecifier
  (return-type [x] ret)
  (arg-types [x] args)
  (owner [x] owner)
  (identifier [x] id))

(defn method-descriptor [spec]
  [:method (return-type spec) (arg-types spec)])

;;;; Method signature matching

(defn subsumes [^objects left
                ^objects right]
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

(defn matching-method
  [args specs]
  (let [args (vec args)]
    (loop [best nil tied? false found-exact? false specs specs]
      (if (nil? specs)
        (if tied?
          (throw (IllegalArgumentException.
                   "More than one matching method found"))
          best)
        (let [curr (first specs)
              curr-types ^objects (arg-types curr)
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
                      (.isAssignableFrom ^Class (return-type best)
                                         (return-type curr)))
                curr best)
              tied? true (next specs))
            (and match? (not found-exact?))
            (cond
              (nil? best)
              (recur curr tied? found-exact? (next specs))
              (subsumes curr-types
                        (arg-types best))
              (recur curr false found-exact? (next specs))
              (Arrays/equals curr-types
                             ^objects (arg-types best))
              (recur
                (if (.isAssignableFrom ^Class (return-type best)
                                       (return-type curr))
                  curr best)
                tied? found-exact? (next specs))
              (not (subsumes (arg-types best) curr-types))
              (recur best true found-exact? (next specs))
              :else
              (recur best tied? found-exact? (next specs)))
            :else (recur best tied? found-exact? (next specs))))))))
