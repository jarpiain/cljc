(ns org.subluminal.binfmt
  (:refer-clojure)
  (:use (clojure.contrib [macro-utils :only (symbol-macrolet)]))
  (:import (java.nio ByteBuffer ByteOrder)
           (clojure.lang Var)))

(import 'java.io.FileInputStream
        'java.nio.channels.FileChannel
        'java.nio.channels.FileChannel$MapMode)

(defn sym->kw [sym]
  (keyword (-> *ns* ns-name name)
           (-> sym name)))

(def *known-types* (atom {}))

(defn lookup [tag]
  (swap! *known-types*
         (fn [ts]
           (if (contains? ts tag)
             ts
             (assoc ts tag {:reader (Var/create)
                            :writer (Var/create)}))))
  (get @*known-types* tag))

(defn bind-tag! [tag rd wr]
  (let [{rd-var :reader wr-var :writer} (lookup tag)]
    (.bindRoot rd-var rd)
    (.bindRoot wr-var wr)))

(defn redef-tag! [tag val]
  (swap! *known-types* assoc tag val))

(defn lookup-reader [tag]
  (get (lookup tag) :reader))

(defn lookup-writer [tag]
  (get (lookup tag) :writer))

(defn read-binary [tag buf & more]
  (apply (lookup-reader tag) buf more))

(defn write-binary [tag buf obj & more]
  (apply (lookup-writer tag) buf obj more))

(defmacro defprimitive [tag [buf obj & args] rd wr]
  (let [keytag (sym->kw tag)
        {inline? :inline} (meta tag)
        rd-source `(~(apply vector
                            (with-meta buf {:tag `ByteBuffer})
                            args)
                    ~rd)
        wr-source `(~(apply vector
                            (with-meta buf {:tag `ByteBuffer})
                            obj
                            args)
                    ~wr)]
    (if inline?
      `(redef-tag! ~keytag {:inline true
                            :reader-source '~rd-source
                            :writer-source '~wr-source
                            :reader (fn ~@rd-source)
                            :writer (fn ~@wr-source)})
      `(bind-tag! ~keytag (fn ~@rd-source) (fn ~@wr-source)))))

(defprimitive ^{:inline true} int8 [buf ^Number obj]
  (.get buf)
  (.put buf (byte obj)))

(defprimitive uint8 [buf ^Number obj]
  (let [b (int (.get buf))]
    (bit-and b 0xFF))
  (let [ub (int obj)
        b (byte (if (> ub 0x7F) (bit-or ub -0x80) ub))]
    (.put buf b)))

(defn buffer-wrap
  "Allocates a ByteBuffer wrapping a seq of (unsigned) bytes"
  [byte-seq]
  (ByteBuffer/wrap (into-array Byte/TYPE
                               (map byte
				    (map #(if (> % 0x7f) (- % 256) %)
					 byte-seq)))))

(defn buffer-unwrap
  "Returns the seq of (unsigned) bytes in a ByteBuffer"
  [buf]
  (let [n (.limit buf)]
    (take n (map #(if (< % 0) (+ % 256) %)
		 (seq (.array buf))))))

(comment
(defmulti read-binary
  "Tries to parse binary data from a ByteBuffer in the specified format"
  (fn [tag & _] tag))

(defmulti write-binary
  "Writes a clojure data structure into a ByteBuffer in the specified
  binary format"
  (fn [tag & _] tag)))

(defn parse-file [tag filename & more]
  (let [file (FileInputStream. (str filename))
        chan (.getChannel file)
        siz (.size chan)
        buf (.map chan FileChannel$MapMode/READ_ONLY 0 siz)]
    (try
      (apply read-binary tag buf more)
      (finally (.close file)))))

(defn invert-map [m]
  (zipmap (vals m) (keys m)))

;;; pseudo-format for fields that should not be serialized

(comment
(defmethod read-binary ::null [_ buf] nil)
(defmethod write-binary ::null [_ buf obj])

;;; primitive types

(defmethod read-binary ::int8
  [_ ^ByteBuffer buf] (.get buf))
(defmethod write-binary ::int8
  [_ ^ByteBuffer buf x] (.put buf (byte x)))
(defmethod read-binary ::uint8
  [_ ^ByteBuffer buf] (let [x (int (.get buf))]
                         (bit-and x (int 0xFF))))
(defmethod write-binary ::uint8
  [_ ^ByteBuffer buf x] (let [ix (int x)]
                           (.put buf (byte (if (>= ix 0x80)
                                             (bit-or -0x80 ix)
                                             ix)))))

(defmethod read-binary ::int16
  [_ ^ByteBuffer buf] (.getShort buf))
(defmethod write-binary ::int16
  [_ ^ByteBuffer buf x] (.putShort buf (short x)))
(defmethod read-binary ::uint16
  [_ ^ByteBuffer buf] (let [x (int (.getShort buf))]
                         (bit-and x (int 0xFFFF))))
(defmethod write-binary ::uint16
  [_ ^ByteBuffer buf x] (.putShort buf (short (if (>= x 0x8000)
                                                 (bit-or -0x8000 x)
                                                 x))))

(defmethod read-binary ::int32
  [_ ^ByteBuffer buf] (.getInt buf))
(defmethod write-binary ::int32
  [_ ^ByteBuffer buf x] (.putInt buf (int x)))
(defmethod read-binary ::uint32
  [_ ^ByteBuffer buf] (let [x (int (.getInt buf))]
                         (bit-and x 0xFFFFFFFF)))
(defmethod write-binary ::uint32
  [_ ^ByteBuffer buf x] (.putInt buf (int (if (>= x 0x80000000)
                                             (bit-or -0x80000000 x)
                                             x))))

(defmethod read-binary ::int64
  [_ ^ByteBuffer buf] (.getLong buf))
(defmethod write-binary ::int64
  [_ ^ByteBuffer buf x] (.putLong buf (long x)))

;; must coerce to bigint to get rid of reflection warning
(defmethod read-binary ::uint64
  [_ ^ByteBuffer buf] (let [x (.getLong buf)]
                         (bit-and (BigInteger/valueOf x) 0xFFFFFFFFFFFFFFFF)))
(defmethod write-binary ::uint64
  [_ ^ByteBuffer buf x] (.putLong buf (long (if (>= x 0x8000000000000000)
                                               (bit-or -0x8000000000000000 x)
                                               x))))

(defmethod read-binary ::single-float
  [_ ^ByteBuffer buf] (.getFloat buf))
(defmethod write-binary ::single-float
  [_ ^ByteBuffer buf x] (.putFloat buf (float x)))

(defmethod read-binary ::double-float
  [_ ^ByteBuffer buf] (.getDouble buf))
(defmethod write-binary ::double-float
  [_ ^ByteBuffer buf x] (.putDouble buf (double x)))


;;; pseudo-formats for selecting byte order within a format declaration

(defmethod read-binary ::set-le-mode
  [_ ^ByteBuffer buf] (.order buf ByteOrder/LITTLE_ENDIAN))

(defmethod write-binary ::set-le-mode
  [_ ^ByteBuffer buf x] (.order buf ByteOrder/LITTLE_ENDIAN))

(defmethod read-binary ::set-be-mode
  [_ ^ByteBuffer buf] (.order buf ByteOrder/BIG_ENDIAN))

(defmethod write-binary ::set-be-mode
  [_ ^ByteBuffer buf x] (.order buf ByteOrder/BIG_ENDIAN)))


(defn kw->sym [kw]
  (symbol (name kw)))

(declare make-reader make-reader-1)
(declare make-writer make-writer-1)
(declare collect-fields collect-aux)

(defn defbin-children [node]
  (when (seq node)
    (case (first node)
      do (next node)
      if (nnext node)
      when (nnext node)
      cond (take-nth 2 (next (next node)))
      case (take-nth 2 (next (nnext node)))
      select (take-nth 2 (next (nnext node)))
      align nil
      skip nil)))

(defn deep-fields [body]
  (filter vector? (tree-seq seq? defbin-children body)))

(defn field-typ [[tag typ]]
  (if (vector? typ)
    (first typ)
    typ))

(defn lookup-accessors [ts]
  (vec (apply concat
         (for [[tag [rd wr]] ts]
           [rd `(lookup-reader ~tag) wr `(lookup-writer ~tag)]))))

(defmacro defbinary [name-ctx & body]
  (let [name (if (vector? name-ctx) (first name-ctx) name-ctx)
        ctx (when (vector? name-ctx) (next name-ctx))
        namekw (sym->kw name)
        gtag (gensym "TAG")
        gbuf (gensym "BUF")
        typs (into #{} (map field-typ (deep-fields `(do ~@body))))
        typs (into {} (for [tag typs] [tag [(gensym "RD") (gensym "WR")]]))]
    `(let ~(lookup-accessors typs)
       (defprimitive ~name ~(apply vector gbuf name ctx)
          (let [~name {}]
            ~(make-reader name gtag gbuf body typs))
          (let ~(vec (apply concat (collect-fields body name)))
            (let ~(vec (apply concat (collect-aux body name)))
              ~(make-writer name gtag gbuf body typs)))))))

(defn- collect-field [fld fmtname]
  (if (keyword? (first fld))
    (let [[tag _ {:keys [aux]}] fld]
      (if aux ()
        (list [(kw->sym tag) `(~fmtname ~tag)])))
    (if (= (first fld) 'do)
      (collect-fields (next fld) fmtname))))

(defn- collect-aux-field [fld fmtname]
  (if (keyword? (first fld))
    (let [[tag _ {:keys [aux]}] fld]
      (if aux
        (list [(kw->sym tag) aux])
        ()))
    (if (= (first fld) 'do)
      (collect-aux (next fld) fmtname))))

(defn- collect-fields [body fmtname]
  (mapcat #(collect-field % fmtname) body))

(defn collect-aux [body fmtname]
  (mapcat #(collect-aux-field % fmtname) body))

(defn- make-reader
  ([name gtag gbuf fields typs] (make-reader name gtag gbuf fields name typs))
  ([name gtag gbuf fields remain typs]
   (reduce #(make-reader-1 name gtag gbuf %2 %1 typs)
           remain
           (reverse fields))))

(defn- make-writer [name gtag gbuf fields typs]
  (reduce #(make-writer-1 name gtag gbuf %2 %1 typs)
          nil
          (reverse fields)))

(defn- binary-source-form [tag args gbuf typs]
  (let [fmt (lookup tag)
        [rd _] (typs tag)]
    (if (:inline fmt)
      (let [[ [buf & more] rd-body] (:reader-source fmt)]
        `(symbol-macrolet ~(apply vector buf gbuf
                                  (interleave more args))
           ~rd-body))
      `(~rd ~gbuf ~@args))))

(defn- binary-dest-form [tag args gbuf fmt-name typs]
  (let [fmt (lookup tag)
        [_ wr] (typs tag)]
    (if (:inline fmt)
      (let [[ [buf obj & more] wr-body] (:writer-source fmt)]
        `(symbol-macrolet ~(apply vector buf gbuf obj fmt-name
                                  (interleave more args))
           ~wr-body))
      `(~wr ~gbuf ~@args))))

(defn- make-field-reader [fmt-name gtag gbuf fld remain typs]
  (let [[tag typ opt] fld
        opt (if (nil? opt) {} opt)

        source-form (or (:transient opt)
                        (if (vector? typ)
                          (binary-source-form (first typ) (next typ) gbuf typs)
                          (binary-source-form typ () gbuf typs)))
        decode-form (cond
                      (:enum opt)
                      `(let [numv# ~source-form
                             symv# (~(invert-map (:enum opt)) numv#)]
                         (or symv# numv#))
                      (:xenum opt)
                      `(let [numv# ~source-form
                             symv# (~(invert-map (:xenum opt)) numv#)]
                         (or symv#
                             (throw (RuntimeException.
                                      (str "illegal enum val: " numv#)))))
                      (:bitmask opt)
                      `(let [numv# ~source-form]
                         (persistent!
                           (reduce #(let [[k# v#] %2]
                                      (if (bit-test numv# k#)
                                        (conj! %1 v#)
                                        %1))
                                   (transient #{})
                                   (list ~@(seq (invert-map (:bitmask opt)))))))
                      :default source-form)
        check-form (cond
                     (:constraint opt)
                     `(let [val# ~decode-form]
                        (or (~(:constraint opt) val#)
                            (throw (RuntimeException.
                                     (str '~fmt-name
                                          " constraint violation: "
                                          '~(:constraint opt)
                                          " not satisfied for "
                                          ~tag " = " val#))))
                        val#)
                     :default decode-form)
        iter-form (cond
                    ;; make sure to force the read ops
                    (:times opt)
                    `(persistent!
                       (reduce conj! (transient [])
                               (repeatedly ~(:times opt) (fn [] ~check-form))))
                    (:until opt)
                    `(let [sentinel# ~(:until opt)]
                       (vec (take-while #(not= % sentinel#)
                                        (repeatedly (fn [] ~check-form)))))
                    :default check-form)
        jump-form (if (:at opt)
                    `(let [curr# (.position ~gbuf)]
                       (.position ~gbuf ~(:at opt))
                       (let [val# ~iter-form]
                         (.position ~gbuf curr#)
                         val#))
                    iter-form)]
    (cond
      (:aux opt)
      `(let [~(kw->sym tag) ~jump-form]
         ~remain)
      (= (first fld) 'internal)
      jump-form
      :default
      `(let [~fmt-name (assoc ~fmt-name ~tag ~jump-form)]
         (let [~(kw->sym tag) (~tag ~fmt-name)]
           ~remain)))))

(defn- make-reader-1 [fmt-name gtag gbuf fld remain typs]
  (cond
    (or (keyword? (first fld)) (= (first fld) 'internal))
    (make-field-reader fmt-name gtag gbuf fld remain typs)

    (= (first fld) 'skip)
    (let [[count] (next fld)]
      `(do (.position ~gbuf
                      (int (+ ~count (.position ~gbuf))))
            ~remain))

    (= (first fld) 'align)
    (let [[align] (next fld)]
      `(do (.position ~gbuf
                      (int (bit-and ~(bit-not (dec align))
                                    (+ (.position ~gbuf)
                                       ~(dec align)))))
         ~remain))

    (= (first fld) 'if)
    (let [[test then else] (next fld)]
      `(let [~fmt-name (if ~test
                         ~(make-reader-1 fmt-name gtag gbuf then fmt-name)
                         ~(if else
                            (make-reader-1 fmt-name gtag gbuf else fmt-name)
                            fmt-name))]
         ~remain))

    (= (first fld) 'cond)
    `(let [~fmt-name (cond
                       ~@(mapcat (fn [[tst fld]]
                                   (list tst (make-reader-1 fmt-name gtag gbuf
                                                            fld fmt-name)))
                              (partition 2 (next fld))))]
       ~remain)

    (= (first fld) 'case)
    (let [[expr & body] (next fld)]
      `(let [~fmt-name (case ~expr
                         ~@(mapcat (fn [[val fld]]
                                     (if fld
                                       (list val (make-reader-1 fmt-name
                                                                gtag gbuf
                                                                fld fmt-name))
                                       (list (make-reader-1 fmt-name gtag gbuf
                                                            val fmt-name))))
                             (partition 2 2 [] body)))]
         ~remain))


    (= (first fld) 'do)
    `(let [~fmt-name ~(make-reader fmt-name gtag gbuf (next fld))]
       ~remain)

    (= (first fld) 'select)
    (let [gselval (gensym "SELV")
          [mask & body] (next fld)]
      `(let [~gselval ~mask]
         ~(make-reader fmt-name gtag gbuf
                       (map (fn [[bit fld]]
                              `(if (~bit ~gselval) ~fld))
                            (partition 2 body))
                       remain)))))

(defn- make-writer-1 [fmt-name tag# gbuf fld remain typs]
  (cond
    (or (keyword? (first fld)) (= (first fld) 'internal))
    (let [[tag typ opt] fld
          opt (if (nil? opt) {} opt)

          source-form (if (= tag 'internal)
                        fmt-name
                        (kw->sym tag))
          encode-form (cond
                        (:enum opt)
                        `(let [symv# ~source-form
                               numv# (~(:enum opt) symv#)]
                           (or numv# symv#))
                        (:xenum opt)
                        `(let [symv# ~source-form
                               numv# (~(:xenum opt) symv#)]
                           (or numv# (throw (RuntimeException. "illegal enum val"))))
                        (:bitmask opt)
                        `(reduce #(bit-or %1 (bit-shift-left 1 (~(:bitmask opt) %2)))
                                 0 ~source-form)
                        :default source-form)
          output-form (if (vector? typ)
                        (binary-dest-form (first typ) (next typ)
                                           gbuf encode-form typs)
                        (binary-dest-form typ () gbuf encode-form typs))
          iter-form (cond
                      (:times opt)
                      `(doseq [~source-form ~source-form] ~output-form)
                      (:until opt)
                      `(do
                         (doseq [~source-form ~source-form] ~output-form)
                         (let [~source-form ~(:until opt)]
                           ~output-form))
                      :default output-form)]
      `(do ~iter-form ~remain))


    (= (first fld) 'skip)
    (let [[count] (next fld)]
      `(do (.position ~gbuf
                      (+ ~count
                         (.position ~gbuf)))
         ~remain))

    (= (first fld) 'align)
    (let [[align] (next fld)]
      `(do (.position ~gbuf
                      (int (bit-and ~(bit-not (dec align))
                                    (+ ~(dec align)
                                       (.position ~gbuf)))))
         ~remain))

    (= (first fld) 'if)
    (let [[test then else] (next fld)]
      `(do (if ~test
             (let ~(vec (apply concat
                               (collect-field then fmt-name)))
               ~(make-writer-1 fmt-name tag# gbuf then fmt-name))
             ~(if else
                `(let ~(vec (apply concat
                                   (collect-field else fmt-name)))
                   ~(make-writer-1 fmt-name tag# gbuf else fmt-name))))
           ~remain))

    (= (first fld) 'cond)
    `(do (cond
           ~@(mapcat (fn [[tst fld]]
                       (list tst
                         `(let ~(vec (apply concat
                                                 (collect-field fld fmt-name)))
                           ~(make-writer-1 fmt-name tag# gbuf
                                           fld fmt-name))))
                     (partition 2 (next fld))))
       ~remain)

    (= (first fld) 'case)
    (let [[expr & body] (next fld)]
      `(do (case ~expr
             ~@(mapcat (fn [[tst fld]]
                         (if fld
                           (list tst
                             `(let ~(vec (apply concat
                                                (collect-field fld fmt-name)))
                                ~(make-writer-1 fmt-name tag# gbuf
                                                fld fmt-name)))
                           (list
                             `(let ~(vec (apply concat
                                               (collect-field tst fmt-name)))
                                ~(make-writer-1 fmt-name tag# gbuf
                                                tst fmt-name)))))
                       (partition 2 2 [] body)))
         ~remain))

    (= (first fld) 'do)
    `(do ~(make-writer fmt-name tag# gbuf (next fld))
         ~remain)

    (= (first fld) 'select)
    (let [[mask & clauses] (next fld)
          gmask (gensym "MASK")]
      `(let [~gmask ~mask]
         ~@(map (fn [[flag fld]]
                  (make-writer-1 fmt-name tag# gbuf
                                 `(if (~gmask ~flag) ~fld)
                                 fmt-name))
                (partition 2 clauses))))))

(defmacro >-
  ([coll key] `(~coll ~key))
  ([coll key & more]
   `(>- (~coll ~key) ~@more)))

(defn bits
  "Returns the number of 1 bits in the non-negative int n"
  [n]
  {:pre (not (neg? n))}
  (loop [c (int 0) x (int n)]
    (if (zero? x)
      c
      (recur (inc c) (int (bit-and x (dec x)))))))

(definline pad4 [^Long x] `(bit-and (+ ~x 3) -4))
(definline padd4 [^Long x] `(let [x# ~x] (- (pad4 x#) x#)))
