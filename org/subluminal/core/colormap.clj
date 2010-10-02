(in-ns 'org.subluminal.xproto)

(define-core-op
  (::create-colormap 4
    [:alloc ::card8 {:xenum {:none 0 :all 1}}]
    [:id ::colormap]
    [:window ::window]
    [:visual ::visual-id]))

(define-core-op
  (::free-colormap 2
    (skip 1)
    [:cmap ::colormap]))

(define-core-op
  (::copy-colormap-and-free 3
    (skip 1)
    [:id ::colormap]
    [:source ::colormap]))

(define-core-op
  (::install-colormap 2
    (skip 1)
    [:cmap ::colormap]))

(define-core-op
  (::uninstall-colormap 2
    (skip 1)
    [:cmap ::colormap]))

(define-core-op
  (::list-installed-colormaps 2
    (skip 1)
    [:window ::window])
  (::list-installed-colormaps-reply
    [:unused ::card8 {:aux 0}]
    [:num-cmaps ::card16 {:aux 0}]
    (skip 22)
    [:cmaps ::colormap {:times num-cmaps}]))

(define-core-op
  (::alloc-color 4
    (skip 1)
    [:cmap ::colormap]
    [:red ::card16]
    [:green ::card16]
    [:blue ::card16]
    (skip 2))
  (::alloc-color-reply
    [:unused ::card8 {:aux 0}]
    [:red ::card16]
    [:green ::card16]
    [:blue ::card16]
    (skip 2)
    [:pixel ::card32]
    (skip 12)))

(define-core-op
  (::alloc-named-color (+ 3 (/ (bin/pad4 (count (:name alloc-named-color))) 4))
    (skip 1)
    [:cmap ::colormap]
    [:name-length ::card16 {:aux (count (:name alloc-named-color))}]
    (skip 2)
    [:name [::ascii 0]]
    (align 4))
  (::alloc-named-color-reply
    [:unused ::card8 {:aux 0}]
    [:pixel ::card32]
    [:exact-red ::card16]
    [:exact-green ::card16]
    [:exact-blue ::card16]
    [:visual-red ::card16]
    [:visual-green ::card16]
    [:visual-blue ::card16]
    (skip 8)))

(define-core-op
  (::alloc-color-cells 3
    [:contiguous ::card8]
    [:cmap ::colormap]
    [:colors ::card16]
    [:planes ::card16])
  (::alloc-color-cells-reply
    [:unused ::card8 {:aux 0}]
    [:num-pixels ::card16 {:aux 0}]
    [:num-masks ::card16 {:aux 0}]
    (skip 20)
    [:pixels ::card32 {:times num-pixels}]
    [:masks ::card32 {:times num-masks}]))

(define-core-op
  (::alloc-color-planes 4
    [:contiguous ::card8]
    [:cmap ::colormap]
    [:colors ::card16]
    [:reds ::card16]
    [:greens ::card16]
    [:blues ::card16])
  (::alloc-color-planes-reply
    [:unused ::card8 {:aux 0}]
    [:num-pixels ::card16 {:aux 0}]
    (skip 2)
    [:red-mask ::card32]
    [:green-mask ::card32]
    [:blue-mask ::card32]
    (skip 8)
    [:pixels ::card32 {:times num-pixels}]))

(define-core-op
  (::free-colors (+ 3 (count (:pixels free-colors)))
    (skip 1)
    [:cmap ::colormap]
    [:plane-mask ::card32]
    [:pixels ::card32 {:times 0}]))

(define-core-op
  (::store-colors (+ 2 (* 3 (count (:items store-colors))))
    (skip 1)
    [:cmap ::colormap]
    [:items ::coloritem {:times 1}]))

(bin/defbinary coloritem
  [:pixel ::card32]
  [:red ::card16]
  [:green ::card16]
  [:blue ::card32]
  [:ops ::card8 {:bitmask {:do-red 0 :do-green 1 :do-blue 2}}]
  (skip 1))

(define-core-op
  (::store-named-color (+ 4 (/ (bin/pad4 (count (:name store-named-color))) 4))
    [:ops ::card8 {:bitmask {:do-red 0 :do-green 1 :do-blue 2}}]
    [:cmap ::colormap]
    [:pixel ::card32]
    [:name-len ::card16 {:aux (count (:name store-named-color))}]
    (skip 2)
    [:name [::ascii 0]]
    (align 4)))

(define-core-op
  (::query-colors (+ 2 (count (:pixels query-colors)))
    (skip 1)
    [:cmap ::colormap]
    [:pixels ::card32 {:times 0}])
  (::query-colors-reply
    [:unused ::card8 {:aux 0}]
    [:num-colors ::card16 {:aux 0}]
    (skip 22)
    [:colors ::rgb {:times num-colors}]))

(bin/defbinary rgb
  [:red ::card16]
  [:green ::card16]
  [:blue ::card16]
  (skip 2))

(define-core-op
  (::lookup-color (+ 3 (/ (bin/pad4 (count (:name lookup-color))) 4))
    (skip 1)
    [:cmap ::colormap]
    [:name-length ::card16 {:aux (count (:name lookup-color))}]
    (skip 2)
    [:name [:ascii 0]]
    (align 4))
  (::lookup-color-reply
    [:unused ::card8 {:aux 0}]
    [:exact-red ::card16]
    [:exact-green ::card16]
    [:exact-blue ::card16]
    [:visual-red ::card16]
    [:visual-green ::card16]
    [:visual-blue ::card16]
    (skip 12)))
