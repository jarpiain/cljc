(in-ns 'org.subluminal.xproto)

; merge with core/window?

(define-core-op
  (::create-pixmap
    [:depth ::card8]
    [:pid *alloc-resource*]
    [:drawable ::drawable]
    [:width ::card16]
    [:height ::card16]))

(define-core-op
  (::free-pixmap
    (skip 1)
    [:pixmap ::pixmap]))
