(ns org.subluminal.util
  (:refer-clojure))

(defn- sat-body [coll form]
  (if-not (sequential? form)
    `(~form ~coll)
    (let [[op & args] form]
      (case op
        --> (let [[x & xs] args]
              (if (seq xs)
                `(if ~(sat-body coll x)
                   ~(sat-body coll (cons '--> xs))
                   true)
                 (sat-body coll x)))
        <-> (let [[lhs rhs] args]
              (sat-body coll
                (list 'and (list '--> lhs rhs)
                      (list '--> rhs lhs))))
        and `(and ~@(map (partial sat-body coll) args))
        or  `(or  ~@(map (partial sat-body coll) args))
        not (let [[x] args]
              `(not ~(sat-body coll x)))
        nand `(not ~(sat-body coll (cons 'and args)))
        nor  `(not ~(sat-body coll (cons 'or  args)))
        xor  `(not ~(sat-body coll (cons '<-> args)))
        ;; First order quantifiers
        all  (let [[x] args]
               `(every? (sat ~x) ~coll))
        some (let [[x] args]
               `(some (sat ~x) ~coll))))))

(defmacro sat
  [form]
  (let [s (gensym)
        body (sat-body s form)]
    `(fn [~s] ~body)))

(defmacro sat?
  [form]
  (let [s (gensym)
        body (sat-body s form)]
    `(fn [~s] (if ~body true false))))
