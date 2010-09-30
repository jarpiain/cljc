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

(define-core-op
  (::change-keyboard-mapping
    (+ 2 (* (:keycode-count change-keyboard-mapping)
            (:keysyms-per-keycode change-keyboard-mapping)))
    [:keycode-count ::card8]
    [:first-keycode ::keycode]
    [:keysyms-per-keycode ::card8]
    (skip 2)
    [:keysyms ::keysym {:times 1}]))

(define-core-op
  (::get-keyboard-mapping 2
    (skip 1)
    [:first-keycode ::keycode]
    [:count ::card8]
    (skip 2))
  (::get-keyboard-mapping-reply
    [:keysyms-per-keycode ::card8]
    (skip 24)
    [:keysyms ::keysym {:times reply-length}]))

(defn get-keyboard-mapping
  ([] (get-keyboard-mapping *display*))
  ([dpy]
   (let [min-k (:min-keycode dpy)
         max-k (:max-keycode dpy)
         count (inc (- max-k min-k))
         kmap @(wait-x dpy ::get-keyboard-mapping
                       {:first-keycode min-k
                        :count count})
         psiz (:keysyms-per-keycode kmap)
         vmap (into [] (concat (take min-k (repeat []))
                               (map vec (partition psiz (:keysyms kmap)))))]
     (dosync
       (ref-set (:keymap dpy) vmap)))))

(defn get-column [code state]
  (if (state :shift)
    1
    0))

(defn keycode->keysym
  ([evt] (keycode->keysym *display* evt))
  ([dpy evt]
   (let [row (:detail evt)
         column (get-column row (:state evt))]
     (get-in @(:keymap dpy) [row column]))))

(defn translate-key
  ([evt] (translate-key *display* evt))
  ([dpy evt]
   (let [ksym (keycode->keysym dpy evt)]
     (cond
       (zero? ksym) :NoSymbol
       (= ksym 0x00FFFFFF) :VoidSymbol

       (or (<= 0x20 ksym 0x7E)
           (<= 0xA0 ksym 0xFF))
       (char ksym)

       (<= 0x1000100 ksym 0x110FFFF)
       (char (- ksym 0x1000000))

       :default ksym))))
