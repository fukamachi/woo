(in-package :cl-user)
(defpackage woo.ev
  (:nicknames :wev)
  (:use :cl)
  (:import-from :woo.ev.tcp
                :tcp-server
                :close-tcp-server)
  (:import-from :woo.ev.socket
                :socket
                :socket-open-p
                :socket-data
                :close-socket
                :write-socket-data
                :write-socket-byte
                :flush-buffer
                :with-async-writing)
  (:import-from :woo.ev.event-loop
                :with-event-loop
                :*buffer-size*)
  (:import-from :woo.ev.condition
                :tcp-error
                :socket-closed)
  (:export :socket
           :socket-open-p
           :with-event-loop
           :tcp-server
           :close-tcp-server
           :write-socket-data
           :write-socket-byte
           :with-async-writing
           :socket-data
           :close-socket
           :*buffer-size*

           ;; conditions
           :tcp-error
           :socket-closed))
(in-package :woo.ev)
