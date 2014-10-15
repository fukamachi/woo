(in-package :cl-user)
(defpackage woo.tcp
  (:use :cl)
  (:import-from :cl-async
                :with-event-loop
                :deref-data-from-pointer
                :check-event-loop-running
                :tcp-server
                :tcp-server-bind-error
                :add-event-loop-exit-callback
                :close-tcp-server
                :tcp-accept-err-cb
                :init-incoming-socket)
  (:import-from :cl-async-base
                :*event-base*
                :event-base-c)
  (:import-from :cl-async-util
                :define-c-callback
                :with-ip-to-sockaddr
                :create-data-pointer
                :free-pointer-data
                :attach-data-to-pointer
                :save-callbacks
                :get-callbacks
                :+bev-opt-close-on-free+)
  (:import-from :libevent2
                :evconnlistener-new-bind
                :evconnlistener-set-error-cb
                :bufferevent-socket-new
                :+lev-opt-reuseable+
                :+lev-opt-close-on-free+)
  (:import-from :lparallel.kernel
                :*kernel*
                :submit-raw-task
                :make-task)
  (:export :tcp-server-parallel))
(in-package :woo.tcp)

(define-c-callback tcp-accept-cb :void ((listener :pointer) (fd :int) (addr :pointer) (socklen :int) (data-pointer :pointer))
  (declare (ignore socklen addr))
  (destructuring-bind (server &key read-cb connect-cb)
      (as::deref-data-from-pointer data-pointer)
    (lparallel.kernel::submit-raw-task
     (lparallel.kernel::make-task
      (lambda ()
        (as:with-event-loop ()
          (let* ((event-base-c (cl-async-base:event-base-c cl-async-base:*event-base*))
                 (bev (le:bufferevent-socket-new event-base-c fd cl-async-util:+bev-opt-close-on-free+)))

            (as::init-incoming-socket bev
                                      (list :read-cb read-cb :connect-cb connect-cb)
                                      server)))))
     lparallel.kernel:*kernel*)))

(defun tcp-server-parallel (bind-address port read-cb event-cb &key connect-cb (backlog -1) stream)
  "Same as cl-async:tcp-server except it sets custom 'tcp-accept-cb'."
  (as::check-event-loop-running)
  (with-ip-to-sockaddr ((sockaddr sockaddr-size) bind-address port)
    (let* ((data-pointer (create-data-pointer))
           (listener (le:evconnlistener-new-bind (event-base-c *event-base*)
                                                 (cffi:callback tcp-accept-cb)
                                                  data-pointer
                                                  (logior le:+lev-opt-reuseable+ le:+lev-opt-close-on-free+)
                                                  backlog
                                                  sockaddr
                                                  sockaddr-size))
           (server-class (make-instance 'tcp-server
                                        :c listener
                                        :stream stream
                                        :data-pointer data-pointer)))
      ;; check that our listener instantiated properly
      (when (or (and (not (cffi:pointerp listener))
                     (zerop listener))
                (cffi:null-pointer-p listener))
        (free-pointer-data data-pointer)
        (error (make-instance 'tcp-server-bind-error :addr bind-address :port port)))
      ;; make sure the server is closed/freed on exit
      (add-event-loop-exit-callback (lambda ()
                                      (close-tcp-server server-class)
                                      (free-pointer-data data-pointer)))
      (attach-data-to-pointer data-pointer (list server-class
                                                 :read-cb read-cb
                                                 :connect-cb connect-cb))
      ;; setup an accept error cb
      (le:evconnlistener-set-error-cb listener (cffi:callback tcp-accept-err-cb))
      (save-callbacks data-pointer (list :event-cb event-cb))
      ;; return the listener, which can be closed by the app if needed
      server-class)))
