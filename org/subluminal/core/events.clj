(in-ns 'org.subluminal.xproto)

(bin/defbinary x-reply
  [:category ::card8 {:enum {:error 0 :reply 1}}]
  (case category
    :error [:err ::x-error]
    :reply (do [:arg ::card8]
               [:serial ::card16]
               [:length ::card32])
    [:event (@(:event-codes *display*) category)]))

(bin/defbinary x-error
  [:code ::card8 {:enum {:request 1 :value 2 :window 3 :pixmap 4 :atom 5
                         :cursor 6 :font 7 :match 8 :drawable 9 :access 10
                         :alloc 11 :colormap 12 :gcontext 13 :id-choice 14
                         :name 15 :length 16 :implementation 17}}]
  [:serial ::card16]
  [:detail ::card32]
  [:minor ::card16]
  [:major ::card8]
  (skip 21))

(defmacro define-x-event [tags & body]
  (let [tags (if (vector? tags) tags [tags])]
   `(do
      ~@(for [tag tags]
          `(bin/defbinary ~(bin/kw->sym tag)
                      [:code ::card8 {:transient ~tag}]
                      [:detail ::card8]
                      [:serial ::card16]
                      ~@body)))))

(def +core-events+
     {2 ::key-press, 3 ::key-release, 4 ::button-press, 5 ::button-release,
      6 ::motion-notify, 7 ::enter-notify, 8 ::leave-notify,
      9 ::focus-in, 10 ::focus-out, 11 ::keymap-notify,
      12 ::expose, 13 ::graphics-exposure, 14 ::no-exposure,
      15 ::visibility-notify, 16 ::create-notify,
      17 ::destroy-notify, 18 ::unmap-notify, 19 ::map-notify,
      20 ::map-request, 21 ::reparent-notify,
      22 ::configure-notify, 23 ::configure-request,
      24 ::gravity-notify, 25 ::resize-request,
      26 ::circulate-notify, 27 ::circulate-request, 28 ::property-notify,
      29 ::selection-clear, 30 ::selection-request,
      31 ::selection-notify, 32 ::colormap-notify,
      33 ::client-message, 34 ::mapping-notify})

(define-x-event [::key-press ::key-release
                 ::button-press ::button-release
                 ::motion-notify]
  [:time ::timestamp]
  [:root ::window]
  [:window ::window]
  [:child ::window]
  [:root-x ::bin/int16]
  [:root-y ::bin/int16]
  [:event-x ::bin/int16]
  [:event-y ::bin/int16]
  [:state ::keybut-mask]
  [:same-screen ::card8]
  (skip 1))

(define-x-event [::enter-notify ::leave-notify]
  [:time ::timestamp]
  [:info ::null {:transient detail
                 :enum {:ancestor 0 :virtual 1 :inferior 2
                        :nonlinear 3 :nonlinear-virtual 4}}]
  [:root ::window]
  [:window ::window]
  [:child ::window]
  [:root-x ::bin/int16]
  [:root-y ::bin/int16]
  [:event-x ::bin/int16]
  [:event-y ::int16]
  [:state ::keybut-mask]
  [:mode ::card8 {:enum {:normal 0 :grab 1 :ungrab 2}}]
  [:same-screen ::card8 {:bitmask {:focus 0 :same-screen 1}}])

(define-x-event [::focus-in ::focus-out]
  [:info ::null {:transient detail
                 :enum {:ancestor 0 :virtual 1 :inferior 2
                        :nonlinear 3 :nonlinear-virtual 4
                        :pointer 5 :pointer-root 6 :none 7}}]
  [:event-widow ::window]
  [:mode ::card8 {:enum {:normal 0 :grab 1 :ungrab 2 :while-grabbed 3}}]
  (skip 23))

;; nonstandard event header
(bin/defbinary keymap-notify
  [:code ::card8 {:transient ::keymap-notify}]
  [:keys ::card8 {:times 31}])

(define-x-event ::expose
  [:window ::window]
  [:x ::card16]
  [:y ::card16]
  [:width ::card16]
  [:height ::card16]
  [:count ::card16]
  (skip 14))

(define-x-event ::graphics-exposure
  [:drawable ::drawable]
  [:x ::card16]
  [:y ::card16]
  [:width ::card16]
  [:height ::card16]
  [:minor-op ::card16]
  [:count ::card16]
  [:major-op ::card8]
  (skip 11))

(define-x-event ::no-exposure
  [:drawable ::drawable]
  [:minor-op ::card16]
  [:major-op ::card8]
  (skip 21))

(define-x-event ::visibility-notify
  [:window ::window]
  [:state ::card8 {:xenum {:unobscured 0 :partially-obscured 1
                           :fully-obscured 2}}]
  (skip 23))

(define-x-event ::create-notify
  [:parent ::window]
  [:window ::window]
  [:x ::bin/int16]
  [:y ::bin/int16]
  [:width ::card16]
  [:height ::card16]
  [:border-width ::card16]
  [:override-redirect ::card8]
  (skip 9))

(define-x-event ::destroy-notify
  [:event-window ::window]
  [:window ::window]
  (skip 20))

(define-x-event [::unmap-notify ::map-notify]
  [:event-window ::window]
  [:window ::window]
  [:info ::card8]
  (skip 19))

(define-x-event ::map-request
  [:parent ::window]
  [:window ::window]
  (skip 20))

(define-x-event ::reparent-notify
  [:event-window ::window]
  [:window ::window]
  [:parent ::window]
  [:x ::bin/int16]
  [:y ::bin/int16]
  [:override-redirect ::card8]
  (skip 11))

(define-x-event ::configure-notify
  [:event-window ::window]
  [:window ::window]
  [:above-sibling ::window]
  [:x ::bin/int16]
  [:y ::bin/int16]
  [:width ::card16]
  [:height ::card16]
  [:border-width ::card16]
  [:override-redirect ::card8]
  (skip 5))

(define-x-event ::configure-request
  [:stack-mode ::card8 {:transient detail
                        :xenum {:above 0 :below 1 :top-if 2
                                :bottom-if 3 :opposite 4}}]
  [:parent ::window]
  [:window ::window]
  [:sibling ::window]
  [:x ::bin/int16]
  [:y ::bin/int16]
  [:width ::card16]
  [:height ::card16]
  [:border-width ::card16]
  [:value-mask ::card16 {:bitmask {:x 0 :y 1 :width 2 :height 3
                                   :border-width 4 :sibling 5 :stack-mode 6}}]
  (skip 4))

(define-x-event ::gravity-notify
  [:event-window ::window]
  [:window ::window]
  [:x ::bin/int16]
  [:y ::bin/int16]
  (skip 16))

(define-x-event ::resize-request
  [:window ::window]
  [:width ::card16]
  [:height ::card16]
  (skip 20))

(define-x-event ::circulate-notify
  [:event-window ::window]
  [:window ::window]
  (skip 4)
  [:place ::card8 {:xenum {:top 0 :bottom 1}}]
  (skip 15))

(define-x-event ::circulate-request
  [:parent ::window]
  [:window ::window]
  (skip 4)
  [:place ::card8 {:xenum {:top 0 :bottom 1}}]
  (skip 15))

(define-x-event ::property-notify
  [:window ::window]
  [:atom ::atom]
  [:time ::timestamp]
  [:state ::card8 {:xenum {:new-value 0 :deleted 1}}]
  (skip 15))

(define-x-event ::selection-clear
  [:time ::timestamp]
  [:owner ::window]
  [:selection ::atom]
  (skip 16))

(define-x-event ::selection-request
  [:time ::timestamp]
  [:owner ::window]
  [:requestor ::window]
  [:selection ::atom]
  [:target ::atom]
  [:property ::atom]
  (skip 4))

(define-x-event ::selection-notify
  [:time ::timestamp]
  [:requestor ::window]
  [:selection ::atom]
  [:target ::atom]
  [:property ::atom]
  (skip 8))

(define-x-event ::colormap-notify
  [:window ::window]
  [:colormap ::colormap]
  [:new? ::card8]
  [:state ::card8 {:enum {:uninstalled 0 :installed 1}}]
  (skip 18))

(define-x-event ::client-message
  [:window ::window]
  [:type ::atom]
  [:data ::card8 {:times 20}])

(define-x-event ::mapping-notify
  [:request ::card8 {:enum {:modifier 0 :keyboard 1 :pointer 2}}]
  [:first-keycode ::keycode]
  [:count ::card8]
  (skip 25))

