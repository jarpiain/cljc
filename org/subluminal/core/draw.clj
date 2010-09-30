(in-ns 'org.subluminal.xproto)

;; graphics contexts

(bin/defbinary gc-valuemask
  [internal ::card32
    {:bitmask {:function 0 :plane-mask 1
               :foreground 2 :background 3
               :line-width 4 :line-style 5
               :cap-style 6 :join-style 7
               :fill-style 8 :fill-rule 9
               :tile 10 :stipple 11
               :tile-stipple-x-origin 12 :tile-stipple-y-origin 13
               :font 14 :subwindow-mode 15
               :graphics-exposures 16
               :clip-x-origin 17 :clip-y-origin 18
               :clip-mask 19 :dash-offset 20
               :dashes 21 :arc-mode 22}}])

(bin/defbinary [gc-values mask]
  (select mask
    :function
    (do [:function ::card8
          {:xenum {:clear 0 :and 1 :and-reverse 2
                   :copy 3 :and-inverted 4 :no-op 5
                   :xor 6 :or 7 :nor 8 :equiv 9
                   :invert 10 :or-reverse 11
                   :copy-inverted 12 :or-inverted 13
                   :nand 14 :set 15}}]
      (skip 3))
    :plane-mask [:plane-mask ::card32]
    :foreground [:foreground ::card32]
    :background [:background ::card32]
    :line-width (do [:line-width ::card16] (skip 2))
    :line-style
    (do [:line-style ::card8
          {:xenum {:solid 0 :on-off-dash 1 :double-dash 2}}]
      (skip 3))
    :cap-style
    (do [:cap-style ::card8
          {:xenum {:not-last 0 :butt 1 :round 2 :projecting 3}}]
      (skip 3))
    :join-style
    (do [:join-style ::card8
          {:xenum {:miter 0 :round 1 :bevel 2}}]
      (skip 3))
    :fill-style
    (do [:fill-style ::card8
          {:xenum {:solid 0 :tiled 1 :stippled 2 :opaque-stippled 3}}]
      (skip 3))
    :fill-rule
    (do [:fill-rule ::card8
          {:xenum {:even-odd 0 :winding 1}}]
      (skip 3))
    :tile [:tile ::pixmap]
    :stipple [:stipple ::pixmap]
    :tile-stipple-x-origin (do [:tile-stipple-x-origin ::bin/int16] (skip 2))
    :tile-stipple-y-origin (do [:tile-stipple-y-origin ::bin/int16] (skip 2))
    :font [:font ::font]
    :subwindow-mode
    (do [:subwindow-mode ::card8
          {:xenum {:clip-by-children 0 :include-inferiors 1}}]
      (skip 3))
    :graphics-exposures (do [:graphics-exposures ::card8] (skip 3))
    :clip-x-origin (do [:clip-x-origin ::bin/int16] (skip 2))
    :clip-y-origin (do [:clip-y-origin ::bin/int16] (skip 2))
    :clip-mask [:clip-mask ::pixmap]
    :dash-offset (do [:dash-offset ::card16] (skip 2))
    :dashes (do [:dashes ::card8] (skip 3))
    :arc-mode
    (do [:arc-mode ::card8
          {:xenum {:chord 0 :pie-slice 1}}]
      (skip 3))))

(define-core-op
  (::create-gc (+ 4 (count (:value-mask create-gc)))
    (skip 1)
    [:cid ::gcontext {:aux *alloc-resource*}]
    [:drawable ::drawable]
    [:value-mask ::gc-valuemask]
    [:values [::gc-values value-mask]]))

(define-core-op
  (::change-gc (+ 3 (count (:value-mask change-gc)))
    (skip 1)
    [:gc ::gcontext]
    [:value-mask ::gc-valuemask]
    [:values [::gc-values value-mask]]))

;; Helper functions to manipulate gcs
(defn create-gc
  ([opts] (create-gc *display* (:root (get-screen)) opts))
  ([wnd opts] (create-gc *display* wnd opts))
  ([dpy wnd opts]
   (alloc-x dpy ::create-gc
            {:drawable wnd
             :value-mask (set (keys opts))
             :values opts})))

(defn change-gc
  ([gc opts] (change-gc *display* gc opts))
  ([dpy gc opts]
   (send-x dpy ::change-gc
           {:gc gc
            :value-mask (set (keys opts))
            :values opts})))

(define-core-op
  (::copy-gc 4
    (skip 1)
    [:source ::gcontext]
    [:dest   ::gcontext]
    [:value-mask ::gc-valuemask]))

(define-core-op
  (::set-dashes (+ 3 (/ (bin/pad4 (:num-dashes set-dashes)) 4))
    (skip 1)
    [:gc ::gcontext]
    [:dash-offset ::card16]
    [:num-dashes ::card16 {:aux (count (:dashes set-dashes))}]
    [:dashes ::card8 {:times 1}]
    (align 4)))

(define-core-op
  (::set-clip-rectangles (+ 3 (* 2 (count (:rectangles set-clip-rectangles))))
    [:ordering ::card8
      {:xenum {:unsorted 0 :y-sorted 1 :yx-sorted 2 :yx-banded 3}}]
    [:gc ::gcontext]
    [:clip-x-origin ::bin/int16]
    [:clip-y-origin ::bin/int16]
    [:rectangles ::rectangle {:times 1}]))

(define-core-op
  (::free-gc 2
    (skip 1)
    [:gc ::gcontext]))

(define-core-op
  (::clear-area 4
    [:exposures ::card8]
    [:window ::window]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:width ::card16]
    [:height ::card16]))

(define-core-op
  (::copy-area 7
    (skip 1)
    [:source ::drawable]
    [:dest   ::drawable]
    [:gc ::gcontext]
    [:src-x ::bin/int16]
    [:src-y ::bin/int16]
    [:dst-x ::bin/int16]
    [:dst-y ::bin/int16]
    [:width ::card16]
    [:height ::card16]))

(define-core-op
  (::copy-plane 8
    (skip 1)
    [:source ::drawable]
    [:dest ::drawable]
    [:gc ::gcontext]
    [:src-x ::bin/int16]
    [:src-y ::bin/int16]
    [:dst-x ::bin/int16]
    [:dst-y ::bin/int16]
    [:width ::card16]
    [:height ::card16]
    [:bit-plane ::card32]))

(define-core-op
  (::poly-point (+ 3 (count (:points poly-point)))
    [:coordinate-mode ::card8 {:xenum {:origin 0 :previous 1}}]
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:points ::point {:times 1}]))

(define-core-op
  (::poly-line (+ 3 (count (:points poly-line)))
    [:coordinate-mode ::card8 {:xenum {:origin 0 :previous 1}}]
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:points ::point {:times 1}]))

(bin/defbinary segment
  [:x1 ::bin/int16]
  [:y1 ::bin/int16]
  [:x2 ::bin/int16]
  [:y2 ::bin/int16])

(define-core-op
  (::poly-segment (+ 3 (* 2 (count (:segments poly-segment))))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:segments ::segment {:times 1}]))

(define-core-op
  (::poly-rectangle (+ 3 (* 2 (count (:rectangles poly-rectangle))))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:rectangles ::rectangle {:times 1}]))

(define-core-op
  (::poly-arc (+ 3 (* 3 (count (:arcs poly-arc))))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:arcs ::arc {:times 1}]))

(define-core-op
  (::fill-poly (+ 4 (count (:points fill-poly)))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:shape ::card8 {:xenum {:complex 0 :nonconvex 1 :convex 2}}]
    [:coordinate-mode ::card8 {:xenum {:origin 0 :previous 1}}]
    (skip 2)
    [:points ::point {:times 1}]))

(define-core-op
  (::poly-fill-rectangle (+ 3 (* 2 (count (:rectangles poly-fill-rectangle))))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:rectangles ::rectangle {:times 1}]))

(define-core-op
  (::poly-fill-arc (+ 3 (* 3 (count (:arcs poly-fill-arc))))
    (skip 1)
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:arcs ::arc {:times 1}]))
