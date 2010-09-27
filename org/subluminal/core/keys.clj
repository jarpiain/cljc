(in-ns 'org.subluminal.xproto)

;; keyboard focus

(define-core-op
  (::set-input-focus 3
    [:revert-to ::card8 {:xenum {:none 0 :pointer-root 1 :parent 2}}]
    [:focus ::window {:enum {:none 0 :pointer-root 1}}]
    [:time ::timestamp]))

(define-core-op
  (::get-input-focus 1 (skip 1))
  (::get-input-focus-reply
    [:revert-to ::card8 {:xenum {:none 0 :pointer-root 1 :parent 2}}]
    [:focus ::window {:enum {:none 0 :pointer-root 1}}]
    (skip 20)))

;; keymap

(define-core-op
  (::query-keymap 1 (skip 1))
  (::query-keymap-reply
    [:unused ::card8 {:aux 0}]
    [:keys ::card8 {:times 32}]))
