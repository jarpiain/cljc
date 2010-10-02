(in-ns 'org.subluminal.xproto)

(def *extensions*
     (assoc *extensions* :bigreq
            {:name "BIG-REQUESTS"
             :ops {::big-req-enable 0 }
             :error-codes {}
             :event-codes {}}))

(define-ext-op :bigreq
  (::big-req-enable 1)
  (::big-req-enable-reply
    [:unused ::card8 {:aux 0}]
    [:maximum-request-length ::card32]
    (skip 20)))
