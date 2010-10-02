(in-ns 'org.subluminal.xproto)

(bin/defbinary shape-kind
  [internal ::card8 {:xenum {:bounding 0 :clip 1 :input 2}}])

(bin/defbinary shape-op
  [internal ::card8 {:xenum {:set 0 :union 1 :intersect 2
                             :subtract 3 :invert 4}}])

(def *extensions*
     (assoc *extensions* :shape
            {:name "SHAPE"
             :version [1 1]
             :ops {::shape-query-version 0
                   ::shape-rectangles 1
                   ::shape-mask 2
                   ::shape-combine 3
                   ::shape-offset 4
                   ::shape-query-extents 5
                   ::shape-select-input 6
                   ::shape-input-selected 7
                   ::shape-get-rectangles 8}
             :error-codes {}
             :event-codes {::shape-notify 0}}))

(define-x-event ::shape-notify
  [:window ::window]
  [:kind ::shape-kind]
  [:x ::bin/int16]
  [:y ::bin/int16]
  [:width ::card16]
  [:height ::card16]
  [:time ::timestamp]
  [:shaped ::card8]
  (skip 11))

(define-ext-op :shape
  (::shape-query-version 1)
  (::shape-query-version-reply
    [:unused ::card8 {:aux 0}]
    [:major-version ::card16]
    [:minor-version ::card16]
    (skip 20)))

(define-ext-op :shape
  (::shape-rectangles (+ 4 (* 2 (count (:rectangles shape-rectangles))))
    [:operation ::shape-op]
    [:kind ::shape-kind]
    [:ordering ::card8 {:xenum {:unsorted 0 :y-sorted 1
                                :yx-sorted 2 :yx-banded 3}}]
    (skip 1)
    [:window ::window]
    [:x-offset ::bin/int16]
    [:y-offset ::bin/int16]
    [:rectangles ::rectangle {:times 1}]))

(define-ext-op :shape
  (::shape-mask 5
    [:operation ::shape-op]
    [:kind ::shape-kind]
    (skip 2)
    [:window ::window]
    [:x-offset ::bin/int16]
    [:y-offset ::bin/int16]
    [:pixmap ::pixmap]))

(define-ext-op :shape
  (::shape-combine 5
    [:operation ::shape-op]
    [:dest-kind ::shape-kind]
    [:src-kind  ::shape-kind]
    (skip 1)
    [:dest ::window]
    [:x-offset ::bin/int16]
    [:y-offset ::bin/int16]
    [:source ::window]))

(define-ext-op :shape
  (::shape-offset 4
    [:kind ::shape-kind]
    (skip 3)
    [:window ::window]
    [:x-offset ::bin/int16]
    [:y-offset ::bin/int16]))

(define-ext-op :shape
  (::shape-query-extents 2
    [:window ::window])
  (::shape-query-extents-reply
    [:unused ::card8 {:aux 0}]
    [:bounding-shaped ::card8]
    [:clip-shaped ::card8]
    (skip 2)
    [:bound-x ::bin/int16]
    [:bound-y ::bin/int16]
    [:bound-width ::card16]
    [:bound-height ::card16]
    [:clip-x ::bin/int16]
    [:clip-y ::bin/int16]
    [:clip-width ::card16]
    [:clip-height ::card16]
    (skip 4)))

(define-ext-op :shape
  (::shape-select-input 3
    [:window ::window]
    [:enable ::card8]
    (skip 3)))

(define-ext-op :shape
  (::shape-input-selected 2
    [:window ::window])
  (::shape-input-selected-reply
    [:enabled ::card8]
    (skip 24)))

(define-ext-op :shape
  (::shape-get-rectangles 3
    [:window ::window]
    [:kind ::shape-kind]
    (skip 3))
  (::shape-get-rectangles-reply
    [:ordering ::card8 {:xenum {:unsorted 0 :y-sorted 1
                                :yx-sorted 2 :yx-banded 3}}]
    [:num-rectangles ::card32 {:aux 0}]
    (skip 20)
    [:rectangles ::rectangle {:times num-rectangles}]))
