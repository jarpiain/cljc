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
  (::no-operation 1
    (skip 1)))
