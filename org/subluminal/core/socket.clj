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

(defn- x-handshake [chan buf]
  (send-channel chan buf ::x-init-block
    {:byte-order :le, :major-version 11, :minor-version 0,
     :protocol-name "", :protocol-data ""})
  (let [result (recv-channel chan buf ::x-init-response)]
    (.clear buf)
    (if (= (:status result) :success)
      (assoc (with-meta (:display result) {:status :success})
             ;; altered upon loading extensions
             :op-codes (ref {})
             :error-codes (ref {})
             :event-codes (ref +core-events+)

             :next-resource-id (ref 0)
             :atoms (ref +predefined-atoms+)
             :atoms-lookup (ref (bin/invert-map +predefined-atoms+))
             :next-serial (ref 0)

             :events (ref (PersistentQueue/EMPTY))
             :promised-events (ref (PersistentQueue/EMPTY))
             :last-error (ref nil)

             :replies (ref {})
             :buffer (agent buf)
             :channel chan)
      result)))

;; Initiate connection and setup display structure

(declare start-x-input)

(defn x-connect [port]
  (let [dpy (x-handshake (x-socket port)
                         (ByteBuffer/allocate 10000))]
    (if (:status (meta dpy))
      (assoc dpy :inbuf (start-x-input dpy))
      dpy)))

;; Terminate connection

(defn x-close
  ([] (x-close *display*))
  ([dpy] (.close (:channel dpy))))

;; Input agent actions

(declare deliver-reply deliver-event)

(defn- x-input-loop [buf chan dpy]
  (when (.isOpen chan)
    (send-off *agent* x-input-loop chan dpy)
    (when (zero? (.remaining buf))
      (.clear buf)
      (.read chan buf)
      (.flip buf))
    (let [reply (bin/read-binary ::x-reply buf)]
      (case (:category reply)
        :reply
        (let [[fmt p :as expect] (dosync
                                   (get @(:replies dpy)
                                        (:serial reply)))
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
            (let [reply-detail (bin/read-binary fmt bigbuf (:arg reply))]
              (deliver-reply dpy (:serial reply) reply-detail))
            (let [len (:length reply)]
              (.position bigbuf
                         (+ (.position bigbuf)
                            24
                            (* 4 len)))))
          bigbuf)

        :error
        (let [serial (get-in reply [:err :serial])
              [fmt p :as expect] (get @(:replies dpy) serial)]
          (println "Was expecting reply: " fmt)
          (println "Got error " (:err reply))
          (if expect
            (deliver-reply dpy serial nil))
          (dosync (ref-set (:last-error dpy)
                           (:err reply)))
          buf)

        (do (deliver-event dpy (:event reply)) buf)))))

(defn- start-x-input [dpy]
  (let [buf (ByteBuffer/allocate 10000)
        ag (agent buf)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (.flip buf) ; limit := 0 -> force read from socket
    (send-off ag x-input-loop (:channel dpy) dpy)
    ag))

;; Send general requests

(defn send-x
  ([tag req] (send-x *display* tag req))
  ([dpy tag req]
   (dosync
     (send-off (:buffer dpy)
               (fn [buf tag req chan]
                 (.clear buf)
                 (bin/write-binary tag buf req)
                 (.flip buf)
                 (.write chan buf)
                 buf)
               tag req (:channel dpy))
     (alter (:next-serial dpy) inc))))

(declare *alloc-resource* *reply-formats*)

(defn alloc-x 
  ([tag req] (alloc-x *display* tag req))
  ([dpy tag req]
     (let [resid (alloc-id dpy)]
       (dosync
         (send-off (:buffer dpy)
                   (fn [buf tag req chan aid]
                     (.clear buf)
                     (binding [*alloc-resource* aid]
                       (bin/write-binary tag buf req))
                     (.flip buf)
                     (.write chan buf)
                     buf)
                   tag req (:channel dpy) resid)
         (alter (:next-serial dpy) inc))
       resid)))

(defn wait-x
  ([tag req] (wait-x *display* tag req))
  ([dpy tag req]
     (if-let [rply (*reply-formats* tag)]
       (dosync
         (let [serial (send-x dpy tag req)
               p (promise)]
           (alter (:replies dpy) assoc (bit-and 0xFFFF serial) [rply p])
           p))
       (throw (IllegalArgumentException.
		(str "No reply format defined for " tag))))))
