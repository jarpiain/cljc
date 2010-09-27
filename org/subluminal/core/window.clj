(in-ns 'org.subluminal.xproto)

(bin/defbinary [window-values mask]
  (select mask
    :background-pixmap [:background-pixmap ::pixmam
                          {:enum {:none 0 :parent-relative 1}}]
    :background-pixel [:background-pixel ::card32]
    :border-pixmap [:border-pixmap ::pixmap {:enum {:copy-from-parent 0}}]
    :border-pixel [:border-pixel ::card32]
    :bit-gravity (do [:bit-gravity ::bitgravity] (skip 3))
    :win-gravity (do [:win-gravity ::wingravity] (skip 3))
    :backing-store
    (do [:backing-store ::card8
           {:enum {:not-useful 0 :when-mapped 1 :always 2}}]
        (skip 3))
    :backing-planes [:backing-planes ::card32]
    :backing-pixel [:backing-pixel ::card32]
    :override-redirect (do [:override-redirect ::card8] (skip 3))
    :save-under (do [:save-under ::card8] (skip 3))
    :event-mask [:event-mask ::eventmask]
    :do-not-propagate-mask [:do-not-propagate-mask ::eventmask]
    :colormap [:colormap ::colormap {:enum {:copy-from-parent 0}}]
    :cursor [:cursor ::cursor {:enum {:none 0}}]))

(bin/defbinary window-value-mask
  [internal ::card32
     {:bitmask {:background-pixmap 0, :background-pixel 1,
                :border-pixmap 2, :border-pixel 3,
                :bit-gravity 4, :win-gravity 5,
                :backing-store 6, :backing-planes 7,
                :backing-pixel 8, :override-redirect 9,
                :save-under 10, :event-mask 11,
                :do-not-propagate-mask 12, :colormap 13, :cursor 14}}])

(define-core-op
  (::create-window (+ 8 (count (:value-mask create-window)))
    [:depth ::card8]
    [:id ::window {:aux *alloc-resource*}]
    [:parent ::window]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:width ::card16]
    [:height ::card16]
    [:border-width ::card16]
    [:class ::card16
       {:enum {:copy-frorm-parent 0 :input-output 1 :input-only 2}}]
    [:visual ::visual-id {:enum {:copy-from-parent 0}}]
    [:value-mask ::window-value-mask]
    [:values [::window-values value-mask]]))

;; Helper to create a window with useful default values
;; Required args: x y width height
(defn create-toplevel-window [args vals]
  (let [vals (merge {:background-pixel (:white-pixel (get-screen))
                     :event-mask #{:key-press :button-press :exposure}}
                     vals)]
    (alloc-x *display* ::create-window
             (merge {:parent (:root (get-screen))
                     :depth (:root-depth (get-screen))
                     :border-width 0
                     :class :input-output
                     :visual :copy-from-parent
                     :value-mask (set (keys vals))
                     :values vals}
                    args))))

(define-core-op
  (::change-window-attributes
    (+ 3 (count (:value-mask change-window-attributes)))
    (skip 1)
    [:window ::window]
    [:value-mask ::window-value-mask]
    [:values [::window-values value-mask]]))

(define-core-op
  (::get-window-attributes 2
    (skip 1)
    [:window ::window])
  (::get-window-attributes-reply
    [:backing-store ::card8 {:enum {:not-useful 0 :when-mapped 1 :always 2}}]
    [:visual ::visual-id]
    [:class ::card16 {:enum {:input-output 1 :input-only 2}}]
    [:bit-gravity ::bitgravity]
    [:win-gravity ::wingravity]
    [:backing-planes ::card32]
    [:backing-pixel ::card32]
    [:save-under ::card8]
    [:map-is-installed ::card8]
    [:map-state ::card8 {:enum {:unmapped 0 :unviewable 1 :viewable 2}}]
    [:override-redirect ::card8]
    [:colormap ::colormap]
    [:all-event-masks ::eventmask]
    [:your-event-mask ::eventmask]
    [:do-not-propagate-mask ::device-event]
    (skip 2)))

(define-core-op
  (::destroy-window 2
    (skip 1)
    [:window ::window]))

(define-core-op
  (::destroy-subwindows 2
    (skip 1)
    [:window ::window]))

(define-core-op
  (::change-save-set 2
    [:mode ::card8 {:xenum {:insert 0 :delete 1}}]
    [:window ::window]))

(define-core-op
  (::reparent-window 4
    (skip 1)
    [:window ::window]
    [:parent ::window]
    [:x ::bin/int16]
    [:y ::bin/int16]))

(define-core-op
  (::map-window 2
    (skip 1)
    [:window ::window]))

(define-core-op
  (::map-subwindows 2
    (skip 1)
    [:window ::window]))

(define-core-op
  (::unmap-window 2
    (skip 1)
    [:window ::window]))

(define-core-op
  (::unmap-subwindows 2
    (skip 1)
    [:window ::window]))

(define-core-op
  (::configure-window (+ 3 (count (:value-mask configure-window)))
    (skip 1)
    [:window ::window]
    [:value-mask ::card16
       {:bitmask {:x 0 :y 1 :width 2 :height 3
                  :border-width 4 :sibling 5 :stack-mode 6}}]
    (skip 2)
    (select value-mask
      :x (do [:x ::bin/int16] (skip 2))
      :y (do [:y ::bin/int16] (skip 2))
      :width (do [:width ::card16] (skip 2))
      :height (do [:height ::card16] (skip 2))
      :border-width (do [:border-width ::card16] (skip 2))
      :sibling [:sibling ::window]
      :stack-mode (do [:stack-mode ::card8
                         {:xenum {:above 0 :below 1 :top-if 2
                                  :bottom-if 3 :opposite 4}}]
                       (skip 3)))))

(define-core-op
  (::circulate-window 2
    [:direction ::card8 {:xenum {:raise-lowest 0 :lower-highest 1}}]
    [:window ::window]))

(define-core-op
  (::get-geometry 2
    (skip 1)
    [:drawable ::drawable])
  (::get-geometry-reply
    [:depth ::card8]
    [:root ::window]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:width ::card16]
    [:height ::card16]
    [:border-width ::card16]
    (skip 10)))

(define-core-op
  (::query-tree 2
    (skip 1)
    [:window ::window])
  (::query-tree-reply
    [:unused ::card8]
    [:root ::window]
    [:parent ::window]
    [:num-children ::card16 {:aux 0}]
    (skip 14)
    [:children ::window {:times num-children}]))
