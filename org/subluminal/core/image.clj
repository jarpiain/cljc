(in-ns 'org.subluminal.xproto)

(define-core-op
  (::put-image (+ 6 (/ (bin/pad4 (count (:data put-image))) 4))
    [:format ::card8 {:xenum {:bitmap 0 :xy-pixmap 1 :z-pixmap 2}}]
    [:drawable ::drawable]
    [:gc ::gcontext]
    [:width ::card16]
    [:height ::card16]
    [:dst-x ::bin/int16]
    [:dst-y ::bin/int16]
    [:left-pad ::card8]
    [:detp ::card8]
    (skip 2)
    [:data ::card8 {:times 1}] ; should use primitive array
    (align 4)))

(define-core-op
  (::get-image 5
    [:format ::card8 {:xenum {:xy-pixmap 1 :z-pixmap 2}}]
    [:drawable ::drawable]
    [:x ::bin/int16]
    [:y ::bin/int16]
    [:width ::card16]
    [:height ::card16]
    [:plane-mask ::card32])
  (::get-image-reply
    [:depth ::card8]
    [:visual ::visual-id]
    (skip 20)
    ; FIXME: need reply length here
    (align 4)))
