(in-ns 'org.subluminal.xproto)

(define-core-op
  (::grab-pointer 6
    [:owner-events ::card8]
    [:window ::grab-window]
    [:event-mask ::device-event]
    [:pointer-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    [:keyboard-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    [:confine-to ::window]
    [:cursor ::cursor]
    [:time ::timestamp])
  (::grab-pointer-reply
    [:status ::card8 {:xenum {:success 0 :already-grabbed 1
                              :invalid-time 2 :not-viewable 3
                              :frozen 4}}]
    (skip 24)))

(define-core-op
  (::ungrab-pointer 2
    (skip 1)
    [:time ::timestamp]))

(define-core-op
  (::grab-button 6
    [:owner-events ::card8]
    [:grab-window ::window]
    [:event-mask ::device-event]
    [:pointer-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    [:keyboard-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    [:confine-to::window]
    [:cursor ::cursor]
    [:button ::card8]
    (skip 1)
    [:modifiers ::keybut-mask]))

(define-core-op
  (::ungrab-button 3
    [:button ::card8]
    [:grab-window ::window]
    [:modifiers ::keybut-mask]
    (skip 2)))
