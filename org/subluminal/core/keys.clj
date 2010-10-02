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
         kmap @(query dpy ::get-keyboard-mapping
                      {:first-keycode min-k
                       :count count})
         psiz (:keysyms-per-keycode kmap)
         vmap (into [] (concat (take min-k (repeat []))
                               (map vec (partition psiz (:keysyms kmap)))))]
     (send dpy assoc :keymap vmap)
     vmap)))

(defn get-column [code state]
  (if (state :shift)
    1
    0))

(defn keycode->keysym
  ([evt] (keycode->keysym *display* evt))
  ([dpy evt]
   (let [row (:detail evt)
         column (get-column row (:state evt))]
     (get-in (:keymap @dpy) [row column]))))

(def *control-keysyms*
     {0xFF08 :backspace
      0xFF09 :tab
      0xFF0A :linefeed
      0xFF0B :clear
      0xFF0D :return
      0xFF13 :pause
      0xFF14 :scroll-lock
      0xFF15 :sys-req
      0xFF1B :escape
      0xFF20 :multi-key
      0xFF21 :kanji
      0xFF22 :muhenkan
      0xFF23 :henkan
      0xFF24 :romaji
      0xFF25 :hiragana
      0xFF26 :katakana
      0xFF50 :home
      0xFF51 :left
      0xFF52 :up
      0xFF53 :right
      0xFF54 :down
      0xFF55 :page-up
      0xFF56 :page-down
      0xFF57 :end
      0xFF58 :begin
      0xFF60 :select
      0xFF61 :print
      0xFF62 :execute
      0xFF63 :insert
      0xFF65 :undo
      0xFF66 :redo
      0xFF67 :menu
      0xFF68 :find
      0xFF69 :cancel
      0xFF6A :help
      0xFF6B :break
      0xFF7E :mode-switch
      0xFF7F :num-lock
      0xFF80 :keypad-space
      0xFF81 :keypad-tab
      0xFF8D :keypad-enter
      0xFFE1 :left-shift
      0xFFE2 :right-shift
      0xFFE3 :left-control
      0xFFE4 :right-control
      0xFFE5 :caps-lock
      0xFFE6 :shift-lock
      0xFFE7 :left-meta
      0xFFE8 :right-meta
      0xFFE9 :left-alt
      0xFFEA :right-alt
      0xFFEB :left-super
      0xFFEC :right-super
      0xFFED :left-hyper
      0xFFEE :right-hyper
      0xFFFF :delete})

(defn translate-keysym [ksym]
  (cond
    (zero? ksym) :NoSymbol
    (= ksym 0x00FFFFFF) :VoidSymbol
    
    (or (<= 0x20 ksym 0x7E)
        (<= 0xA0 ksym 0xFF))
    (char ksym)
    
    (<= 0x1000100 ksym 0x110FFFF)
    (char (- ksym 0x1000000))
    
    :default (if-let [ctl (*control-keysyms* ksym)]
               ctl
               ksym)))

(defn translate-key
  ([evt] (translate-key *display* evt))
  ([dpy evt]
   (translate-keysym (keycode->keysym dpy evt))))
