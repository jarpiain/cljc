(in-ns 'org.subluminal.xproto)

(derive ::region ::bin/int32)

(bin/defbinary window-region-kind
  [internal ::card8 {:xenum {:bounding 0 :clip 1}}])

(def *extensions*
     (assoc *extensions* :fixes
            {:name "XFIXES"
             :version [4 0]
             :ops {::fixes-query-version 0
                   ::fixes-change-save-set 1
                   ::select-selection-input 2
                   ::select-cursor-input 3
                   ::get-cursor-image 4
                   ::create-region 5
                   ::create-region-from-bitmap 6
                   ::create-region-from-window 7
                   ::create-region-from-picture 8
                   ::destroy-region 9
                   ::set-region 10
                   ::copy-region 11
                   ::union-region 12
                   ::intersect-region 13
                   ::subtract-region 14
                   ::invert-region 15
                   ::translate-regoin 16
                   ::region-extents 17
                   ::fetch-region 18
                   ::set-gc-clip-region 19
                   ::set-window-shape-region 20
                   ::set-picture-clip-region 21
                   ::set-cursor-name 22
                   ::get-cursor-name 23
                   ::get-cursor-image-and-name 24
                   ::change-cursor 25
                   ::change-cursor-by-name 26
                   ::expand-region 27
                   ::hide-cursor 28
                   ::show-cursor 29}
             :error-codes {}
             :event-codes {::selection-notify 0
                           ::cursor-notify 1}}))

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

(define-ext-op :fixes
  (::fixes-query-version 3
    [:client-major-version ::card32]
    [:client-minor-version ::card32])
  (::shape-query-version-reply
    [:unused ::card8 {:aux 0}]
    [:major-version ::card32]
    [:minor-version ::card32]
    (skip 16)))

