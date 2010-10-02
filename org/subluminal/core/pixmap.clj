(in-ns 'org.subluminal.xproto)

; merge with core/window?

(define-core-op
  (::create-pixmap 4
    [:depth ::card8]
    [:id ::pixmap]
    [:drawable ::drawable]
    [:width ::card16]
    [:height ::card16]))

(define-core-op
  (::free-pixmap 2
    (skip 1)
    [:pixmap ::pixmap]))
