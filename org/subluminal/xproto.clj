(ns org.subluminal.xproto
  (:refer-clojure)
  (:import (java.nio ByteBuffer ByteOrder
                     BufferOverflowException)
	   (java.nio.channels SocketChannel)
	   (java.net InetSocketAddress)
     (java.util.concurrent LinkedBlockingQueue TimeUnit)
	   (clojure.lang PersistentQueue))
  (:require (org.subluminal [binfmt :as bin])))

(declare *display*)

;; Core protocol
(load "core/types")
(load "core/events")
(load "core/display")
(load "core/socket")
(load "core/request")

;; Core protocol requests categorized
(load "core/window")
(load "core/atoms")
(load "core/grab")
(load "core/pointer")
(load "core/keys")
(load "core/font")
(load "core/pixmap")
(load "core/draw")
(load "core/image")
(load "core/text")
(load "core/colormap")
(load "core/ext")
