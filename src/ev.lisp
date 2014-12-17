(in-package :cl-user)
(defpackage woo.ev
  (:nicknames :wev)
  (:use :cl)
  (:import-from :woo.ev.tcp
                :tcp-server
                :close-tcp-server
                :*connection-timeout*)
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
                :*buffer-size*
                :*evloop*)
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
           :*connection-timeout*
           :*evloop*

           ;; conditions
           :tcp-error
           :socket-closed

           ;; Extensions to ev
           :ev_signal_set
           :ev_signal_init))
(in-package :woo.ev)

(defun ev_signal_set (ev signum)
  (setf (cffi:foreign-slot-value ev 'ev::ev_signal 'ev::signum) signum))

(defun ev_signal_init (ev cb signum)
  (ev::ev_init ev cb)
  (ev_signal_set ev signum))
