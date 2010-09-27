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
    [:confine-to ::window]
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

(define-core-op
  (::change-active-pointer-grab 4
    (skip 1)
    [:cursor ::cursor]
    [:time ::timestamp]
    [:event-mask ::device-event]
    (skip 2)))

(define-core-op
  (::grab-keyboard 4
    [:owner-events ::card8]
    [:grab-window ::window]
    [:time ::timestamp]
    [:pointer-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    [:keyboard-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    (skip 2))
  (::grab-keyboard-reply
    [:status ::card8 {:xenum {:success 0 :already-grabbed 1
                              :invalid-time 2 :not-viewable 3
                              :frozel 4}}]
    (skip 24)))

(define-core-op
  (::ungrab-keyboard 2
    (skip 1)
    [:time ::timestamp]))

(define-core-op
  (::grab-key 4
    [:owner-events ::card8]
    [:grab-window ::window]
    [:modifiers ::keymask]
    [:key ::keycode]
    [:pointer-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    [:keyboard-mode ::card8 {:xenum {:synchronous 0 :asynchronous 1}}]
    (skip 3)))

(define-core-op
  (::ungrab-key 3
    [:key ::keycode]
    [:grab-window ::window]
    [:modifiers ::keymask]
    (skip 2)))

(define-core-op
  (::allow-events 2
    [:mode ::card8 {:xenum {:async-pointer 0 :sync-pointer 1 :replay-pointer 2
                            :async-keyboard 3 :sync-keyboard 4
                            :replay-keyboard 5 :async-both 6 :sync-both 7}}]
    [:time ::timestamp]))

(define-core-op
  (::grab-server 1 (skip 1)))

(define-core-op
  (::ungrab-server 1 (skip 1)))
