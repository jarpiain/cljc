(in-ns 'org.subluminal.xproto)

(define-core-op
  (::create-colormap 4
    [:alloc ::card8 {:xenum {:none 0 :all 1}}]
    [:mid ::colormap {:aux *alloc-resource*}]
    [:window ::window]
    [:visual ::visual-id]))

(define-core-op
  (::free-colormap 2
    (skip 1)
    [:cmap ::colormap]))

(define-core-op
  (::copy-colormap-and-free 3
    (skip 1)
    [:mid ::colormap {:aux *alloc-resource*}]
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
