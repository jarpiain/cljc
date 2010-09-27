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
