;; Pointer-related other than grabs

(in-ns 'org.subluminal.xproto)

(define-core-op
  (::query-pointer 2
    (skip 1)
    [:window ::window])
  (::query-pointer-reply
    [:same-screen ::card8]
    [:root ::window]
    [:child ::window]
    [:root-x ::bin/int16]
    [:root-y ::bin/int16]
    [:win-x ::bin/int16]
    [:win-y ::bin/int16]
    [:mask ::device-event]
    (skip 6)))

(define-core-op
  (::get-motion-events 4
    (skip 1)
    [:window ::window]
    [:start ::timestamp]
    [:stop ::timestamp])
  (::get-motion-events-reply
    [:unused ::card8 {:aux 0}]
    [:num-events ::card32 {:aux 0}]
    (skip 20)
    [:events ::timecoord {:times num-events}]))

(bin/defbinary timecoord
  [:time ::timestamp]
  [:x ::bin/int16]
  [:y ::bin/int16])

(define-core-op
  (::warp-pointer 6
    (skip 1)
    [:src-window ::window]
    [:dst-window ::window]
    [:src-x ::bin/int16]
    [:src-y ::bin/int16]
    [:src-width ::card16]
    [:src-height ::card16]
    [:dst-x ::bin/int16]
    [:dst-y ::bin/int16]))

;; cursor

(define-core-op
  (::create-cursor 8
    (skip 1)
    [:id ::cursor]
    [:source ::pixmap]
    [:mask ::pixmap]
    [:fore-red ::card16]
    [:fore-green ::card16]
    [:fore-blue ::card16]
    [:back-red ::card16]
    [:back-green ::card16]
    [:back-blue ::card16]
    [:x ::card16]
    [:y ::card16]))

(define-core-op
  (::create-glyph-cursor 8
    (skip 1)
    [:id ::cursor]
    [:source-font ::font]
    [:mask-font ::font]
    [:source-char ::card16]
    [:mask-char ::card16]
    [:fore-red ::card16]
    [:fore-green ::card16]
    [:fore-blue ::card16]
    [:back-red ::card16]
    [:back-green ::card16]
    [:back-blue ::card16]))

(define-core-op
  (::free-cursor 2
    (skip 1)
    [:cursor ::cursor]))

(define-core-op
  (::recolor-cursor 5
    (skip 1)
    [:cursor ::cursor]
    [:fore-red ::card16]
    [:fore-green ::card16]
    [:fore-blue ::card16]
    [:back-red ::card16]
    [:back-green ::card16]
    [:back-blue ::card16]))

(define-core-op
  (::change-pointer-control 3
    (skip 1)
    [:acceleration-numerator ::bin/int16]
    [:acceleration-denominator ::bin/int16]
    [:threshold ::bin/int16]
    [:do-acceleration ::card8]
    [:do-threshold ::card8]))

(define-core-op
  (::get-pointer-control 1
    (skip 1))
  (::get-pointer-control-reply
    [:unused ::card8 {:aux 0}]
    [:acceleration-numerator ::card16]
    [:acceleration-denominator ::card16]
    [:threshold ::card16]
    (skip 18)))
