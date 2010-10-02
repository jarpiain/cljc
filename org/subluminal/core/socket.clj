(in-ns 'org.subluminal.xproto)

(defn- x-socket [dpy]
  (SocketChannel/open
    (InetSocketAddress. "127.0.0.1" (+ 6000 dpy))))

(defn- send-channel [^SocketChannel chan
                    ^ByteBuffer buf
                    tag pkt]
  (bin/write-binary tag buf pkt)
  (.flip buf)
  (.write chan buf)
  (.clear buf))

(defn- recv-channel [^SocketChannel chan
                    ^ByteBuffer buf
                    tag]
  (.read chan buf)
  (.flip buf)
  (bin/read-binary tag buf))

(defn- x-handshake [chan ^ByteBuffer buf]
  (send-channel chan buf ::x-init-block
    {:byte-order :le, :major-version 11, :minor-version 0,
     :protocol-name "", :protocol-data ""})
  (let [result (recv-channel chan buf ::x-init-response)]
    (.clear buf)
    (if (= (:status result) :success)
      (assoc (with-meta (:display result) {:status :success})
             ;; altered upon loading extensions
             :extensions {}
             :op-codes {}
             :error-codes {}
             :event-codes +core-events+

             :next-resource-id 0
             :atoms +predefined-atoms+
             :atoms-lookup (bin/invert-map +predefined-atoms+)
             :next-serial 1

             :events (LinkedBlockingQueue.)
             :last-error (ref nil)

             :keymap nil
             :replies (LinkedBlockingQueue.)
             :buffer buf
             :channel chan)
      result)))

(defn- seek-reply [dpy serial]
  (let [^BlockingQueue que (:replies @dpy)]
    (loop [rply (.peek que)]
      (when rply
        (let [[ser fmt p] rply]
          (cond
            (= ser serial) rply

            (< ser serial)
            (do (.poll que) (recur (.peek que)))

            :else
            nil))))))

(defn- deliver-reply [dpy serial reply]
  (when-let [[ser fmt p] (seek-reply dpy serial)]
    (let [^BlockingQueue q (:replies @dpy)]
      (deliver p reply)
      (.poll q))))

;; Initiate connection and setup display structure

(declare start-x-input)

(defn x-connect [port]
  (let [dpy (x-handshake (x-socket port)
                         (ByteBuffer/allocate 10000))
        agt (agent dpy)]
    (if (:status (meta dpy))
      (send agt assoc :inbuf (start-x-input agt))
      dpy)))

;; Terminate connection

(defn x-close [dpy]
  (.close (:channel dpy)))

;; Input agent actions

(declare deliver-reply deliver-event)

(defn- clear-buffer [^ByteBuffer buf ^SocketChannel chan]
  (when (< (.remaining buf) 32)
    (let [arr (make-array Byte/TYPE 32)
          siz (.remaining buf)]
      (.get buf arr 0 siz)
      (.clear buf)
      (.put buf arr 0 siz)
      (.read chan buf)
      (.flip buf))))

(defn- x-input-loop [^ByteBuffer buf ^SocketChannel chan dpy]
  (when (.isOpen chan)
    (send-off *agent* x-input-loop chan dpy)
    (clear-buffer buf chan)
    (let [reply (bin/read-binary ::x-reply buf)]
      (case (:category reply)
        :reply
        (let [[_ fmt p :as expect] (seek-reply dpy (:serial reply))
               szdiff (- (.capacity buf)
                         (-> reply :length (* 4) (+ 32)))
               bigbuf (if (>= szdiff 0) buf
                        (ByteBuffer/allocate (- (.capacity buf) szdiff)))]
          (when (< szdiff 0)
            (.put bigbuf (.array buf))
            (.position bigbuf (.capacity buf))
            (while (< (.position bigbuf) (.capacity bigbuf))
              (.read chan bigbuf))
            (.order bigbuf (.order buf))
            (.position bigbuf (.position buf)))
          (if expect
            (let [reply-detail (bin/read-binary fmt bigbuf
                                                (:arg reply)
                                                (:length reply))]
              (deliver-reply dpy (:serial reply) reply-detail))
            (let [len (:length reply)]
              (.position bigbuf
                         (+ (.position bigbuf)
                            24
                            (* 4 len)))))
          bigbuf)

        :error
        (let [serial (get-in reply [:err :serial])
              [fmt p :as expect] (get @(:replies @dpy) serial)]
          (println "Was expecting reply: " fmt)
          (println "Got error " (:err reply))
          (if expect
            (deliver-reply dpy serial nil))
          (dosync (ref-set (:last-error @dpy)
                           (:err reply)))
          buf)

        ; event
        (let [code ((:event-codes @dpy) (:category reply))
              evt (bin/read-binary code buf)]
          (do (deliver-event dpy evt) buf))))))

(defn- start-x-input [dpy]
  (let [buf (ByteBuffer/allocate 10000)
        ag (agent buf)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (.flip buf) ; limit := 0 -> force read from socket
    (send-off ag x-input-loop (:channel @dpy) dpy)
    ag))

;; Send general requests

(defn flush-display [dpy]
  (let [^ByteBuffer buf (:buffer dpy)
        ^SocketChannel chan (:channel dpy)]
    (when-not (zero? (.position buf))
      (.flip buf)
      (.write chan buf)
      (.clear buf)))
  dpy)

(defn- check-out-buf [dpy]
  (let [^ByteBuffer buf (:buffer dpy)]
    (when (< (.remaining buf) 32)
      (flush-display dpy))))

(defn- request-action [dpy tag req]
  (let [^ByteBuffer buf (:buffer dpy)]
    (check-out-buf dpy)
    (let [pos (.position buf)]
      (try
        (bin/write-binary tag buf req)
        (catch BufferOverflowException e
          (.position buf (+ pos 2))
          (let [siz (bin/read-binary ::card16 buf)]
            (.position buf pos)
            (flush-display dpy)
            (bin/write-binary tag buf req)))))
    (update-in dpy [:next-serial] inc)))

(defn request
  ([tag req] (request *display* tag req))
  ([dpy tag req]
   (send-off dpy request-action tag req)))

(defn alloc
  ([tag req] (alloc *display* tag req))
  ([dpy tag req]
   (send-off dpy
     (fn [dpy]
       (let [aid (alloc-id dpy)]
         (update-in
           (request-action dpy tag (merge req {:id aid}))
           [:next-resource-id] inc))))))

(declare *reply-formats*)

(defn query
  ([tag req] (query *display* tag req))
  ([dpy tag req]
     (if-let [rply (*reply-formats* tag)]
       (let [p (promise)]
         (send-off dpy
           (fn [dpy]
             (.put ^BlockingQueue (:replies dpy)
                   [(bit-and 0xFFFF (:next-serial dpy)) rply p])
             (flush-display (request-action dpy tag req))))
         p)
       (throw (IllegalArgumentException.
                (str "No reply format defined for " tag))))))

(defn query-more
  ([tag req] (query-more *display* tag req))
  ([dpy tag req]
     (if-let [rply (*reply-formats* tag)]
       (let [ps (repeatedly promise)]
         (send-off dpy
           (fn [dpy]
             (dosync
               (alter (:replies dpy)
                      assoc (bit-and 0xFFFF (:next-serial dpy)) [rply ps]))
             (flush-display (request-action dpy tag req))))
         ps)
       (throw (IllegalArgumentException.
                (str "No reply format defined for " tag))))))
