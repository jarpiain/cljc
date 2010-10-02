(in-ns 'org.subluminal.xproto)

(def *reply-formats* {})

(defmacro define-ext-op
  ([op] (define-ext-op op nil))
  ([op reply]
   (let [[major minor len & body] op]
     `(do
        ~(when reply
           `(def *reply-formats*
                 (assoc *reply-formats*
                        ~minor
                        ~(first reply))))
        `(bin/defbinary ~(bin/kw->sym minor)
           [:maj ::card8 {:aux (-> *display* :extensions deref
                                   ~major :op)}]
           [:min ::card8 {:aux ~(-> *extensions* ~major :ops ~minor)}]
           [:length ::card16 {:aux ~len}]
           ~@body)))))

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
    [:allow-exposures ::card8 {:xenum {:no 0 :yes 1 :default 2}}]))

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
  (::no-operation 1
    (skip 1)))
