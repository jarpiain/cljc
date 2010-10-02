(in-ns 'org.subluminal.xproto)

(define-core-op
  (::intern-atom (+ 2 (/ (bin/pad4 (count (:name intern-atom)))
                         4))
    [:only-if-exists ::card8]
    [:name-length ::card16 {:aux (count (:name intern-atom))}]
    (skip 2)
    [:name [::ascii name-length]]
    (align 4))
  (::intern-atom-reply
    [:unused ::card8 {:aux 0}]
    [:atom ::atom]
    (skip 20)))

(define-core-op
  (::get-atom-name 2
    (skip 1)
    [:atom ::atom])
  (::get-atom-name-reply
    [:unused ::card8 {:aux 0}]
    [:name-length ::card16 {:aux 0}]
    (skip 22)
    [:name [::ascii name-length]]
    (align 4)))

(defn intern-atom
  ([kw] (intern-atom *display* kw))
  ([dpy kw]
   (if-let [at (get (:atoms @dpy) kw)]
     at
     (let [reply @(query dpy ::intern-atom
                         {:only-if-exists 0
                          :name (name kw)})]
       (when reply
         (send dpy intern-atom-action kw (:atom reply))
         (:atom reply))))))

(defn atom-name
  ([a] (atom-name *display* a))
  ([dpy a]
   (if-let [kw (get (:atoms-lookup @dpy) a)]
     kw
     (let [reply @(query dpy ::get-atom-name {:atom a})]
       (when reply
         (let [kw (keyword (:name reply))]
           (send dpy intern-atom-action kw a)
           kw))))))

;; -- window properties

(define-core-op
  (::change-property
    (let [bits (:format change-property)
          nums (count (:data change-property))
          bytes (/ (* bits nums) 8)]
      (+ 6 (bin/pad4 bytes)))
    [:mode ::card8 {:xenum {:replace 0 :prepend 1 :append 2}}]
    [:window ::window]
    [:property ::atom]
    [:type ::atom]
    [:format ::card8]
    (skip 3)
    [:data-len ::card32 {:aux (count (:data change-property))}]
    [:data (case format
             8  ::card8
             16 ::card16
             32 ::card32)
           {:times data-len}]
    (align 4)))

(define-core-op
  (::delete-property 3
    (skip 1)
    [:window ::window]
    [:property ::atom]))

(define-core-op
  (::get-property 6
    [:delete ::card8]
    [:window ::window]
    [:property ::atom]
    [:type ::atom] ; 0 -> any type
    [:long-offset ::card32]
    [:long-length ::card32])
  (::get-property-reply
    [:format ::card8]
    [:type ::atom]
    [:bytes-after ::card32]
    [:data-len ::card32]
    (skip 12)
    (if (not= format 0)
      (do [:data (case format
                    8 ::card8
                   16 ::card16
                   32 ::card32)
                 {:times data-len}]
           (align 4)))))

(define-core-op
  (::list-properties 2
    (skip 1)
    [:window ::window])
  (::list-properties-reply
    [:unused ::card8 {:aux 0}]
    [:num-atoms ::card16 {:aux 0}]
    (skip 22)
    [:atoms ::atom {:times num-atoms}]))

(define-core-op
  (::rotate-properties (+ 3 (count (:properties rotate-properties)))
    (skip 1)
    [:window ::window]
    [:num-properties ::card16 {:aux (count (:properties rotate-properties))}]
    [:delta ::bin/int16]
    [:properties ::atom {:times 1}]))

;; Todo: get actual property values
(defn get-properties
  ([wnd] (get-properties *display* wnd))
  ([dpy wnd]
   (let [reply @(query dpy ::list-properties {:window wnd})]
     (when reply
       (let [keys (doall (map #(atom-name dpy %) (:atoms reply)))
             typs (doall
                    (map #(-> (query dpy ::get-property
                                  {:delete 0
                                   :window wnd
                                   :property (intern-atom dpy %)
                                   :type 0
                                   :long-offset 0
                                   :long-length 0})
                              deref :type)
                         keys))
             vals (doall (map (partial atom-name dpy) typs))]
         (zipmap keys vals))))))

(defmulti interpret-property :type)
(defmethod interpret-property :STRING
  [prop] (->> prop :data
              (map char)
              (apply str)))

;; -- selections

(define-core-op
  (::set-selection-owner 4
    (skip 1)
    [:owner ::window]
    [:selection ::atom]
    [:time ::timestamp]))

(define-core-op
  (::get-selection-owner 2
    (skip 1)
    [:selection ::atom])
  (::get-selection-owner-reply
    [:unused ::card8 {:aux 0}]
    [:owner ::window]
    (skip 20)))

(define-core-op
  (::convert-selection 6
    (skip 1)
    [:requestor ::window]
    [:selection ::atom]
    [:target ::atom]
    [:property ::atom]
    [:time ::timestamp]))
