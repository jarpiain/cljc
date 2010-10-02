(in-ns 'org.subluminal.xproto)

(bin/defbinary x-init-block
  (:byte-order ::card8 {:enum {:be 0x42 :le 0x6c}
                        :default :le})
  (if (= byte-order :le)
    (:set-order ::bin/set-le-mode {:aux 0}))
  (skip 1)
  (:major-version ::card16 {:default 11})
  (:minor-version ::card16 {:default 0})
  (:protocol-name-len ::card16 {:aux (count (:protocol-name x-init-block))})
  (:protocol-data-len ::card16 {:aux (count (:protocol-data x-init-block))})
  (skip 2)
  (:protocol-name [::ascii protocol-name-len])
  (align 4)
  (:protocol-data [::ascii protocol-data-len])
  (align 4))

(bin/defbinary x-init-response
  (:status ::card8 {:xenum {:failed 0 :success 1 :authenticate 2}})
  (case status
    :failed
    (do (:reason-len ::card8)
        (:protocol-major-version ::card16)
        (:protocol-minor-version ::card16)
        (:extra-data-len ::card16)
        (:reason [::ascii reason-len])
        (align 4))

    :authenticate
    (do (skip 5)
        (:extra-data-len ::card16)
        (:reason [::ascii (* 4 extra-data-len)]))

    :success
    (do (skip 1)
        (:protocol-major-version ::card16)
        (:protocol-minor-version ::card16)
        (:display ::display))))

(bin/defbinary display
  [:extra-len ::card16]
  [:release-number ::card32]
  [:resource-id-base ::card32]
  [:resource-id-mask ::card32]
  [:motion-buffer-size ::card32]
  [:vendor-len ::card16 {:aux (-> display :vendor count)}]
  [:max-request-len ::card16]
  [:num-screens ::card8 {:aux (-> display :screens count)}]
  [:num-formats ::card8 {:aux (-> display :pixmap-formats count)}]
  [:image-byte-order ::card8 {:xenum {:le 0, :be 1}}]
  [:bitmap-bit-order ::card8 {:xenum {:le 0, :be 1}}]
  [:bitmap-scanline-unit ::card8]
  [:bitmap-scanline-pad  ::card8]
  [:min-keycode ::keycode]
  [:max-keycode ::keycode]
  (skip 4)
  [:vendor [::ascii vendor-len]]
  (align 4)
  [:pixmap-formats ::pixmap-format {:times num-formats}]
  [:screens ::screen {:times num-screens}])

(bin/defbinary pixmap-format
  [:depth ::card8]
  [:bits-per-pixel ::card8]
  [:scanline-pad ::card8]
  (skip 5))

(bin/defbinary screen
  [:root ::window]
  [:default-colormap ::colormap]
  [:white-pixel ::card32]
  [:black-pixel ::card32]
  [:current-input-masks ::eventmask]
  [:width-pix ::card16]
  [:height-pix ::card16]
  [:width-mm ::card16]
  [:height-mm ::card16]
  [:min-installed-maps ::card16]
  [:max-installed-maps ::card16]
  [:root-visual ::visual-id]
  [:backing-stores ::card8 {:enum {:never 0 :when-mapped 1 :always 2}}]
  [:save-unders ::card8]
  [:root-depth ::card8]
  [:num-depths ::card8]
  [:depths ::depth {:times num-depths}])

(bin/defbinary depth
  [:dep ::card8]
  (skip 1)
  [:num-visuals ::card16]
  (skip 4)
  [:visuals ::visual {:times num-visuals}])

(bin/defbinary visual
  [:id ::visual-id]
  [:class ::card8 {:enum {:static-gray 0 :gray-scale 1 :static-color 2
                          :pseudo-color 3 :true-color 4 :direct-color 5}}]
  [:bits-per-rgb ::card8]
  [:colormap-entries ::card16]
  [:red-mask   ::card32]
  [:green-mask ::card32]
  [:blue-mask  ::card32]
  (skip 4))

(defn get-screen
  ([] (get-screen *display*))
  ([dpy] (get (:screens @dpy) 0)))

(defn- intern-atom-action [dpy k v]
  (assoc dpy
         :atoms (assoc (:atoms dpy) k v)
         :atoms-lookup (assoc (:atoms-lookup dpy) v k)))

(defn- gen-resource-id [base mask idx]
  (let [unit (bit-and (int mask)
                      (inc (bit-not (int mask))))]
    (+ (int base)
       (* (int idx) unit))))

(defn alloc-id
  ([] (alloc-id *display*))
  ([dpy] (gen-resource-id (:resource-id-base dpy)
                          (:resource-id-mask dpy)
                          (:next-resource-id dpy))))

(defn next-event [dpy]
  (.poll (:events @dpy)))

(defn wait-event
  ([dpy] (.take (:events @dpy)))
  ([dpy timeout]
   (.poll (:events @dpy) timeout TimeUnit/MILLISECONDS)))

(defn deliver-event [dpy evt]
  (.put (:events @dpy) evt))

;(defn deliver-reply [dpy serial reply]
;  (let [[fmt p :as expect] (get @(:replies @dpy) serial)]
;    (deliver (if (seq? p)
;               (first p)
;               p)
;             reply)
;    (if (seq? p)
;      (dosync (alter (:replies @dpy) update-in [serial 1] rest))
;      (dosync (alter (:replies @dpy) dissoc serial)))))
