(in-package :cl-user)
(defpackage woo.ev
  (:nicknames :wev)
  (:use :cl)
  (:import-from :woo.ev.tcp
                :tcp-server
                :close-tcp-server)
  (:import-from :woo.ev.socket
                :socket
                :socket-closed-p
                :socket-data
                :close-socket
                :write-socket-data)
  (:import-from :woo.ev.event-loop
                :with-event-loop
                :*buffer-size*)
  (:import-from :woo.ev.condition
                :tcp-error
                :socket-closed)
  (:export :socket
           :socket-closed-p
           :with-event-loop
           :tcp-server
           :close-tcp-server
           :write-socket-data
           :socket-data
           :close-socket
           :*buffer-size*

           ;; conditions
           :tcp-error
           :socket-closed))
(in-package :woo.ev)
