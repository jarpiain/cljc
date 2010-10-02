(in-ns 'org.subluminal.xproto)

(def *reply-formats* {})

(defmacro define-ext-op
  ([major op] `(define-ext-op ~major ~op nil))
  ([major op reply]
   (let [[minor len & body] op]
     `(do
        ~(when reply
           `(def *reply-formats*
                 (assoc *reply-formats*
                        ~minor
                        ~(first reply))))
        (bin/defbinary ~(bin/kw->sym minor)
           [:maj ::card8 {:aux (get-in *display*
                                       [:extensions ~major :major-opcode])}]
           [:min ::card8 {:aux (get-in *extensions* [~major :ops ~minor])}]
           [:length ::card16 {:aux ~len}]
           ~@body)
        ~(when reply
           (let [[minr [ff ftyp fopt] & body] reply
                 gdet (gensym)
                 lsym 'reply-length]
             `(bin/defbinary [~(bin/kw->sym minr) ~gdet ~lsym]
                [~ff ~ftyp ~(merge fopt {:transient gdet})]
                ~@body)))))))


(defmacro define-core-op
  ([op] `(define-core-op ~op nil))
  ([op reply]
   `(do
      ~(when reply
         `(def *reply-formats*
               (assoc *reply-formats*
                      ~(first op)
                      ~(first reply))))
      ~(let [[tag len detail & body] op]
         `(bin/defbinary ~(bin/kw->sym tag)
            [:op ::card8 {:aux ~(+core-ops+ tag)}]
            ~detail
            [:length ::card16 {:aux ~len}]
            ~@body))
      ~(when reply
         (let [[tag [ff ftyp fopt] & body] reply
               gdet (gensym)
               lsym 'reply-length]
           `(bin/defbinary [~(bin/kw->sym (first reply)) ~gdet ~lsym]
              [~ff ~ftyp ~(merge fopt {:transient gdet})]
              ~@body))))))

;; Miscellaneous requests

(define-core-op
  (::send-event 11
    [:propagate ::card8]
    [:destination ::window {:enum {:pointer-window 0
                                   :input-focus 1}}]
    [:event-mask ::eventmask]
    [:event ::card8 {:times 32}]))

(define-core-op
  (::query-best-size 3
    [:class ::card8 {:xenum {:cursor 0 :tile 1 :stipple 2}}]
    [:drawable ::drawable]
    [:width ::card16]
    [:height ::card16])
  (::query-best-size-reply
    [:unused ::card8 {:aux 0}]
    [:width ::card16]
    [:height ::card16]
    (skip 20)))

(define-core-op
  (::bell 1
    [:percent ::bin/int8]))

(define-core-op
  (::set-screen-saver 3
    (skip 1)
    [:timeout ::bin/int16]
    [:interval ::bin/int16]
    [:prefer-blanking ::card8 {:xenum {:no 0 :yes 1 :default 2}}]
    [:allow-exposures ::card8 {:xenum {:no 0 :yes 1 :default 2}}]
    (skip 2)))

(define-core-op
  (::get-screen-saver 1
    (skip 1))
  (::get-screen-saver-reply
    [:unused ::card8 {:aux 0}]
    [:timeout ::card16]
    [:interval ::card16]
    [:prefer-blanking ::card8]
    [:allow-exposures ::card8]
    (skip 18)))

(define-core-op
  (::force-screen-saver 1
    [:mode ::card8 {:xenum {:reset 0 :activate 1}}]))

(define-core-op
  (::change-hosts (+ 2 (/ (bin/pad4 (count (:address change-hosts))) 4))
    [:mode ::card8 {:xenum {:insert 0 :delete 1}}]
    [:family ::card8 {:xenum {:internet 0 :decnet 1 :chaos 2}}]
    (skip 1)
    [:address-length ::card8 {:aux (count (:address change-hosts))}]
    [:address ::card8 {:times 1}]
    (align 4)))

(define-core-op
  (::list-hosts 1
    (skip 1))
  (::list-hosts-reply
    [:mode ::card8 {:xenum {:disabled 0 :enabled 1}}]
    [:num-hosts ::card16 {:aux 0}]
    (skip 22)
    [:hosts ::host {:times num-hosts}]))

(define-core-op
  (::set-access-control 1
    [:mode ::card8 {:xenum {:disable 0 :enable 1}}]))

(define-core-op
  (::set-close-down-mode 1
    [:mode ::card8
       {:xenum {:destroy 0
                :retain-permanent 1
                :retain-temporary 2}}]))

(define-core-op
  (::kill-client 2
    (skip 1)
    [:resource ::card32]))

(define-core-op
  (::no-operation 1
    (skip 1)))
