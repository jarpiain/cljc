(ns org.subluminal.util
  (:use (clojure.contrib monads))
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

(def parser-m (state-t maybe-m))

(defn match-char
  "Parser that matches a specified char"
  [ch]
  (fn [^String input]
    (if (= (first input) ch)
      [ch (.substring input 1)]
      nil)))

(defn peek-char
  "Parser that fails if next char doesn't match. Doesn't consume input."
  [chs]
  (fn [^String input]
    (when-let [ch (chs (first input))]
      [ch input])))

(def
  ^{:doc "Parser that matches until end of input"}
  match-tail
  (fn [input]
    [input ""]))

(defn match-until
  "Parser that matches until next occurrence of a terminator char.
   Consumes the terminator."
  [ch]
  (fn [^String input]
    (let [idx (.indexOf input (int ch))]
      (if (== idx -1)
        nil
        [(.substring input 0 idx) (.substring input (inc idx))]))))

(defn peek-until
  "Parser that matches until next occurrence of one of a set of chars.
   Does not consume the terminator."
  [chs]
  (fn [^String input]
    (let [res (apply str (take-while (complement chs)
                                     input))]
      [res (.substring input (count res))])))

(defn match-case
  "Parser that matches the keys in a map returning the corresponding value"
  [map]
  (fn [^String input]
    (let [ch (first input)]
      (if (contains? map ch)
        [(map ch) (.substring input 1)]
        nil))))

(defn optional [parser]
  (with-monad parser-m
    (m-plus parser (m-result nil))))

(defn match-+
  "Match a sequence of one or more instances of p"
  [p]
  (domonad parser-m
    [head p
     tail (optional (match-+ p))]
    (cons head tail)))

(defn match-*
  "Match a sequence of zero or more instances of p"
  [p]
  (with-monad parser-m
    (m-plus (match-+ p) (m-result ()))))
