(in-package :cl-user)
(defpackage woo.ev
  (:nicknames :wev)
  (:use :cl)
  (:import-from :woo.ev.tcp
                :tcp-server
                :close-tcp-server
                :with-sockaddr
                :*connection-timeout*)
  (:import-from :woo.ev.socket
                :socket
                :socket-open-p
                :socket-remote-addr
                :socket-remote-port
                :socket-data
                :close-socket
                :write-socket-data
                :write-socket-byte
                :flush-buffer
                :with-async-writing)
  (:import-from :woo.ev.event-loop
                :with-event-loop
                :*buffer-size*
                :*evloop*
                :*data-registry*)
  (:import-from :woo.ev.condition
                :tcp-error
                :socket-closed)
  (:export :socket
           :socket-open-p
           :socket-remote-addr
           :socket-remote-port
           :with-event-loop
           :tcp-server
           :close-tcp-server
           :write-socket-data
           :write-socket-byte
           :with-async-writing
           :socket-data
           :close-socket
           :*buffer-size*
           :*connection-timeout*
           :*evloop*
           :*data-registry*
           :with-sockaddr

           ;; conditions
           :tcp-error
           :socket-closed))
(in-package :woo.ev)
