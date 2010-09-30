(in-ns 'org.subluminal.xproto)

(bin/defbinary text-item-8
  [:strlen ::card8]
  (if (= strlen 255)
    (do [:b3 ::card8]
        [:b2 ::card8]
        [:b1 ::card8]
        [:b0 ::card8])
    (do [:delta ::bin/int8]
        [:string [::ascii 0]])))

(bin/defbinary text-item-16
  [:strlen ::card8]
  (if (= strlen 255)
    (do [:b3 ::card8]
        [:b2 ::card8]
        [:b1 ::card8]
        [:b0 ::card8])
    (do [:delta ::bin/int8]
        [:string [::string16 0]])))

(defn item-len8 [tx]
  (if (= (:strlen tx) 255)
    5
    (+ 2 (:strlen tx))))

(defn item-len16 [tx]
  (if (= (:strlen tx) 255)
    5
    (+ 2 (* 2 (:strlen tx)))))

(defn mktext
  ([txt] (mktext txt 0))
  ([txt d] {:strlen (count txt)
            :delta d
            :string txt}))

(defn mk-font-shift [font]
  {:strlen 255
   :b3 (bit-and 0xFF (bit-shift-right font 24))
   :b2 (bit-and 0xFF (bit-shift-right font 16))
   :b1 (bit-and 0xFF (bit-shift-right font 8))
   :b0 (bit-and 0xFF font)})

(define-core-op
  (::poly-text8 (->> (:items poly-text8)
                     (map item-len8)
                     (reduce +)
                     bin/pad4
                     #(/ % 4)
                     (+ 4))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:items ::text-item-8 {:times 1}]
    (align 4)))

(define-core-op
  (::poly-text16 (->> (:items poly-text16)
                      (map item-len16)
                      (reduce +)
                      bin/pad4
                      #(/ % 4)
                      (+ 4))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gc]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:items ::text-item-16 {:times 1}]
    (align 4)))

(define-core-op
  (::image-text8 (+ 4 (/ (bin/pad4 (count (:string image-text8))) 4))
    [:strlen ::card8 {:aux (count (:string image-text8))}]
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:string [::ascii 0]]
    (align 4)))

(define-core-op
  (::image-text16 (+ 4 (/ (bin/pad4 (* 2 (:strlen image-text16))) 4))
    [:strlen ::card8 {:aux (count (:string image-text16))}]
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:string [::string16 0]]
    (align 4)))
