(in-package :cl-user)
(defpackage woo.signal
  (:use :cl)
  (:import-from :woo.multiprocess
                :*listener*
                :child-worker-pids)
  (:export :sigint-cb
           :sigquit-cb
           :sigchld-cb))
(in-package :woo.signal)

(cffi:defcallback sigquit-cb :void ((evloop :pointer) (signal :pointer) (events :int))
  (declare (ignore signal events))
  (format t "~&[~D] Stopping a server...~%" (wsys:getpid))

  ;; Stop accepting new clients.
  (lev:ev-io-stop evloop *listener*)

  ;; Stop child processes.
  (dolist (pid (child-worker-pids))
    (wsys:kill pid 3))
  (setf (child-worker-pids) '())

  ;; Wait until all requests are served or passed 10 sec.
  (loop
    repeat 100
    until (= (hash-table-count wev:*data-registry*) 0)
    do (sleep 0.1))

  ;; Close existing all sockets.
  (maphash (lambda (fd socket)
             (vom:warn "Force closing a socket (fd=~D) in ~D." fd (wsys:getpid))
             (wev:close-socket socket))
           wev:*data-registry*)

  ;; Stop all events.
  (lev:ev-break evloop lev:+EVBREAK-ALL+)

  (format t "~&[~D] Bye.~%" (wsys:getpid))

  #+sbcl
  (sb-ext:exit)
  #-sbcl
  (cl-user::quit))

(cffi:defcallback sigint-cb :void ((evloop :pointer) (signal :pointer) (events :int))
  (declare (ignore signal events))
  (format t "~&[~D] Stopping a server immediately...~%" (wsys:getpid))

  ;; Stop all events.
  (lev:ev-break evloop lev:+EVBREAK-ALL+)

  ;; Stop child processes.
  (dolist (pid (child-worker-pids))
    (wsys:kill pid 2))
  (setf (child-worker-pids) '())

  (format t "~&[~D] Bye.~%" (wsys:getpid))

  #+sbcl
  (sb-ext:exit)
  #-sbcl
  (cl-user::quit))

(cffi:defcallback sigchld-cb :void ((evloop :pointer) (signal :pointer) (events :int))
  (declare (ignore signal events))
  (format t "~&[~D] SIGCHILD~%" (wsys:getpid))
  (let ((pid #+sbcl (sb-posix:fork)
             #-sbcl (wsys:fork)))
    (if (zerop pid)
        (format t "Worker started: ~A~%" (wsys:getpid))
        (push pid (child-worker-pids)))))
