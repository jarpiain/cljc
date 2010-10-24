;; Copyright (c) Juha Arpiainen. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'org.subluminal.compiler)

;;;; Utilities related mostly to casting and coercing between types

;; Used as a pseudo-type for expressions that don't return normally,
;; e.g., recur.
#_(def Bottom (class (fn Bottom [])))

(defn prim-type? [^Class c]
  (or #_(= c Bottom)
      (and c (.isPrimitive c) (not (= c Void/TYPE)))))

(defn ref-type? [^Class c]
  (or (nil? c)
      (not (.isPrimitive c))))

(defn local-prim-type?
  "Allowed primitive types for a local binding"
  [^Class c]
  (or (= c Long/TYPE)
      (= c Double/TYPE)))

(defn load-op [^Class c]
  (cond
    (or (= c Byte/TYPE) (= c Boolean/TYPE) (= c Character/TYPE)
        (= c Short/TYPE) (= c Integer/TYPE))
    :iload
    (= c Float/TYPE)
    :fload
    (= c Long/TYPE)
    :lload
    (= c Double/TYPE)
    :dload
    :else :aload))

(defn store-op [^Class c]
  (cond
    (or (= c Byte/TYPE) (= c Boolean/TYPE) (= c Character/TYPE)
        (= c Short/TYPE) (= c Integer/TYPE))
    :istore
    (= c Float/TYPE)
    :fstore
    (= c Long/TYPE)
    :lstore
    (= c Double/TYPE)
    :dstore
    :else :astore))

(defn promoted-type [^Class c]
  {:pre [(prim-type? c)]}
  (cond
    (= c Integer/TYPE) Long/TYPE
    (= c Short/TYPE) Long/TYPE
    (= c Byte/TYPE) Long/TYPE
    (= c Long/TYPE) Long/TYPE
    (= c Float/TYPE) Double/TYPE
    (= c Double/TYPE) Double/TYPE
    (= c Character/TYPE) Object
    (= c Boolean/TYPE) Object))

(defn valid-promotion?
  [^Class from ^Class to]
  (cond
    (= to Void/TYPE) true
    ;(= from Bottom) true
    (= from to) (prim-type? from)
    (= to Long/TYPE)
    (or (= from Integer/TYPE)
        (= from Short/TYPE)
        (= from Byte/TYPE))
    (= to Integer/TYPE)
    (or (= from Short/TYPE)
        (= from Byte/TYPE))
    (= to Short/TYPE)
    (= from Byte/TYPE)
    (= to Double/TYPE)
    (= from Float/TYPE)
    :else false))

;; If analyze is called with second argument of 'want
;; and the result has :gen-type 'have,
;; the two types should satisfy this
(defn analyze-contract [want have]
  (or (true? want) #_(= have Bottom)
      (and (= have Void/TYPE) (= want Void/TYPE))
      (and (nil? want) (ref-type? have))
      (and (prim-type? want)
           (or (ref-type? have)
               (= want have)))))

(defn gen-contract [source gen]
  (or (ref-type? gen) #_(= source Bottom)
      (valid-promotion? source gen)))

(def boxed-version
  {Byte/TYPE Byte
   Short/TYPE Short
   Boolean/TYPE Boolean
   Integer/TYPE Integer
   Long/TYPE Long
   Float/TYPE Float
   Double/TYPE Double})

(defn boxit [prim]
  {:pre [(prim-type? prim)]}
  (cond
    (= prim Byte/TYPE)
    [:invokestatic [Byte 'valueOf [:method Byte [:byte]]]]
    (= prim Short/TYPE)
    [:invokestatic [Short 'valueOf [:method Short [:short]]]]
    (= prim Integer/TYPE)
    [:invokestatic [Integer 'valueOf [:method Integer [:int]]]]
    (= prim Long/TYPE)
    [:invokestatic [Long 'valueOf [:method Long [:long]]]]
    (= prim Float/TYPE)
    [:invokestatic [Float 'valueOf [:method Float [:float]]]]
    (= prim Double/TYPE)
    [:invokestatic [Double 'valueOf [:method Double [:double]]]]
    (= prim Boolean/TYPE)
    [:invokestatic [Boolean 'valueOf [:method Boolean [:boolean]]]]
    (= prim Character/TYPE)
    [:invokestatic [Character 'valueOf [:method Character [:char]]]]))

(defn unbox-it [prim]
  {:pre [(prim-type? prim)]}
  (cond
    (= prim Byte/TYPE)
    [:invokestatic [RT 'byteCast [:method :byte [Object]]]]
    (= prim Short/TYPE)
    [:invokestatic [RT 'shortCast [:method :short [Object]]]]
    (= prim Integer/TYPE)
    [:invokestatic [RT 'intCast [:method :int [Object]]]]
    (= prim Long/TYPE)
    [:invokestatic [RT 'longCast [:method :long [Object]]]]
    (= prim Float/TYPE)
    [:invokestatic [RT 'floatCast [:method :float [Object]]]]
    (= prim Double/TYPE)
    [:invokestatic [RT 'doubleCast [:method :double [Object]]]]
    (= prim Character/TYPE)
    [:invokestatic [RT 'charCast [:method :char [Object]]]]
    (= prim Boolean/TYPE)
    [:invokestatic [RT 'booleanCast [:method :boolean [Object]]]]))

(defn maybe-cast [^Class from ^Class to]
  {:pre [(ref-type? from) to (ref-type? to)]}
  (cond
    (nil? from) ()
    (isa? from to) ()
    :else [[:checkcast to]]))

(defn maybe-pop
  "Generate bytecode to pop the operand stack"
  [src]
  (cond
    (= src Void/TYPE) ()
    (ref-type? src)
    [[:pop]]
    (local-prim-type? src) ; long double
    [[:pop2]]
    :else
    [[:pop]]))

;; Generate primitive promotions, boxing, ref type casts
(defn gen-convert
  [source gen]
  {:pre [(gen-contract source gen)]}
  (cond
    ;(= source Bottom) ()
    (= gen Void/TYPE) (maybe-pop source)
    (= source gen) ()
    (ref-type? gen)
    (cond
      (= source Void/TYPE)
      `([:aconst-null])
      (prim-type? source)
      (let [box (boxed-version source)]
        `(~(boxit source)
          ~@(maybe-cast box gen)))
      :else
      (maybe-cast source gen))
    :else ; (valid-promotion? source gen)
    (cond
      (= gen Long/TYPE)
      `([:i2l])
      (= gen Double/TYPE)
      `([:f2d])
      :else ())))

(defn gen-coerce
  [gen need]
  (cond
    ;(= gen Bottom) ()
    (= need Void/TYPE) (maybe-pop gen)
    (ref-type? need)
    (cond
      (= gen Void/TYPE) [[:aconst-null]]
      (ref-type? gen) (maybe-cast gen need)
      :else ; primitive
      (let [box (boxed-version gen)]
        `(~(boxit gen)
          ~@(maybe-cast box need))))
    ;; Need is primitive
    (valid-promotion? gen need)
    (gen-convert gen need)
    (ref-type? gen)
    (list (unbox-it need))
    ;; Lossy primitive conversion
    :else
    `(~(boxit gen)
      ~(unbox-it need))))

(defn box-args
  [types argvals]
  (into-array Object (map (fn [typ arg]
                            (if (ref-type? typ) arg
                              (condp = typ
                                Boolean/TYPE (boolean arg)
                                Character/TYPE (char arg)
                                Byte/TYPE (byte arg)
                                Short/TYPE (short arg)
                                Integer/TYPE (int arg)
                                Long/TYPE (long arg)
                                Float/TYPE (float arg)
                                Double/TYPE (double arg))))
                          (seq types) argvals)))

;; --> unboxed values on stack
(defn gen-args [ptypes args]
  (mapcat (fn [^Class t arg]
            `(~@(gen arg)
              ~@(gen-coerce (:gen-type arg) t)))
          (seq ptypes)
          args))

;; --> Object[]
(defn gen-array [args]
  `([:sipush ~(short (count args))]
    [:anewarray ~Object]
    ~@(mapcat (fn [arg i]
                `([:dup]
                  [:sipush ~(short i)]
                  ~@(gen arg)
                  ~@(when (prim-type? (:gen-type arg))
                      (list (boxit (:gen-type arg))))
                  [:aastore]))
              args
              (range))))
