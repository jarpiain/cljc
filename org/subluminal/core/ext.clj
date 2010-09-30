(in-ns 'org.subluminal.xproto)

(def *extensions* {})

(define-core-op
  (::query-extension (+ 2 (/ (bin/pad4 (count (:name query-extension))) 4))
    (skip 1)
    [:name-length ::card16 {:aux (count (:name query-extension))}]
    (skip 2)
    [:name [::ascii 0]]
    (align 4))
  (::query-extension-reply
    [:unused ::card8 {:aux 0}]
    [:present ::card8]
    [:major-opcode ::card8]
    [:first-event ::card8]
    [:first-error ::card8]
    (skip 20)))

(defn query-extension
  "name should be an unqualified keyword"
  ([name-kw] (query-extension *display* name-kw))
  ([dpy name-kw]
   (if-let [info (-> dpy :extensions deref name-kw)]
     info
     (let [{evt :first-event 
            err :first-error
            op :first-op
            :as info}
            (wait-x dpy ::query-extension {:name (name (name name-kw))})]
       (dosync
         (alter (-> dpy :extensions)
                assoc name-kw info)
         (alter (-> dpy :event-codes)
                into (map (fn [k v] [(+ v evt) k])
                          (:error-codes (*extensions* name-kw))))
         (alter (-> dpy :error-codes)
                into (map (fn [k v] [(+ v err) k])
                          (:error-codes (*extensions* name-kw))))
         info)))))

; ext/foo.clj:
;
; (def *extensions*
;      (assoc *extensions* :FOO
;             {:ops {0 ::create-foo 1 ::destroy-foo}
;              :error-codes {::foo-error 0}
;              :event-codes {::foo-event 0}}))
;
; (define-x-event [::foo-event]
;   [:data ::datatype]
;   ...)
;
; (define-ext-op :FOO
;   (::create-foo len
;     [:data ::datatype]
;     ...)
;   (::create-foo-reply
;     [:data ::datatype]
;     ...))

(define-core-op
  (::list-extensions 1
    (skip 1))
  (::list-extensions-reply
    [:num-names ::card8 {:aux 0}]
    (skip 24)
    [:names ::ascii {:times num-names}]
    (align 4)))
