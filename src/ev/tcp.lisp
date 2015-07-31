(in-package :cl-user)
(defpackage woo.ev.tcp
  (:use :cl)
  (:import-from :woo.ev.event-loop
                :check-event-loop-running
                :deref-data-from-pointer
                :callbacks
                :*evloop*
                :*input-buffer*)
  (:import-from :woo.ev.socket
                :make-socket
                :close-socket
                :socket-read-cb
                :socket-read-watcher
                :socket-timeout-timer
                :socket-last-activity)
  (:import-from :woo.ev.condition
                :os-error)
  (:import-from :woo.syscall
                :set-fd-nonblock
                #+nil :close
                #+nil :read
                :errno
                :EWOULDBLOCK
                :ECONNABORTED
                :ECONNREFUSED
                :ECONNRESET
                :EPROTO
                :EINTR)
  (:import-from :woo.llsocket
                #-linux :accept
                #+linux :accept4
                :bind
                #+nil :listen
                :+SOCK-CLOEXEC+
                :+SOCK-NONBLOCK+
                :socket
                :sockaddr-in
                :inet-ntoa
                :setsockopt
                :+AF-INET+
                :+SOCK-STREAM+
                :+SOL-SOCKET+
                :+SO-REUSEADDR+)
  (:import-from :woo.ev.util
                :define-c-callback
                :io-fd)
  (:import-from :lev
                :ev-io
                :ev-now
                :ev-io-init
                :ev-io-start
                :ev-io-stop
                :ev-timer
                :ev-timer-init
                :ev-timer-again
                :+EV-READ+
                :+EV-TIMER+)
  (:import-from :swap-bytes
                :htonl
                :htons)
  (:import-from :cffi
                :foreign-alloc
                :foreign-free
                :foreign-slot-value
                :with-foreign-object
                :with-foreign-slots
                :mem-aref
                :foreign-type-size)
  (:import-from :split-sequence
                :split-sequence)
  (:export :tcp-server
           :close-tcp-server
           :*connection-timeout*))
(in-package :woo.ev.tcp)

(declaim (type double-float *connection-timeout*))
(defvar *connection-timeout* (coerce (* 15 60) 'double-float))

(define-c-callback tcp-read-cb :void ((evloop :pointer) (watcher :pointer) (events :int))
  (declare (ignore evloop events))
  (let* ((fd (io-fd watcher))
         (buffer-len (length *input-buffer*))
         (socket (deref-data-from-pointer fd))
         (read-cb (socket-read-cb socket)))
    (loop
      (let ((n (wsys:read fd (static-vectors:static-vector-pointer *input-buffer*) buffer-len)))
        (declare (dynamic-extent n))
        (case n
          (-1
           (let ((errno (wsys:errno)))
             (cond
               ((or (= errno wsys:EWOULDBLOCK)
                    (= errno wsys:EINTR)))
               ((or (= errno wsys:ECONNABORTED)
                    (= errno wsys:ECONNREFUSED)
                    (= errno wsys:ECONNRESET))
                (vom:error "Connection is already closed (Code: ~D)" errno)
                (close-socket socket))
               (t
                (vom:error "Unexpected error (Code: ~D)" errno)
                (close-socket socket))))
           (return))
          (0
           ;; EOF
           (setf (socket-last-activity socket) (lev:ev-now *evloop*))
           (close-socket socket)
           (return))
          (otherwise
           (setf (socket-last-activity socket) (lev:ev-now *evloop*))
           (when read-cb
             (funcall (the function read-cb) socket *input-buffer* :start 0 :end n))
           (unless (= n buffer-len)
             (return))))))))

(define-c-callback timeout-cb :void ((evloop :pointer) (timer :pointer) (events :int))
  (declare (ignore events))
  (let* ((now (lev:ev-now evloop))
         (fd (io-fd (cffi:foreign-slot-value timer '(:struct lev:ev-timer) 'lev::data)))
         (socket (deref-data-from-pointer fd))
         (timeout (+ (socket-last-activity socket) *connection-timeout*)))
    (declare (type double-float now timeout))
    (if (< timeout now)
        (progn
          (vom:info "Timeout, closing connection")
          (close-socket socket))
        (progn
          (setf (cffi:foreign-slot-value timer '(:struct lev:ev-timer) 'lev::repeat)
                (- timeout now))
          (lev:ev-timer-again evloop timer)))))

(defvar *dummy-sockaddr* (cffi:foreign-alloc '(:struct wsock:sockaddr-in)))
(defvar *dummy-socklen* (cffi:foreign-alloc 'wsock:socklen-t))
(wsys:bzero *dummy-sockaddr* (cffi:foreign-type-size '(:struct wsock:sockaddr-in)))
(setf (cffi:mem-aref *dummy-socklen* 'wsock:socklen-t) (cffi:foreign-type-size '(:struct wsock:sockaddr-in)))

(define-c-callback tcp-accept-cb :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore events))
  (let* ((fd (io-fd listener))
         (client-fd #+linux (wsock:accept4 fd
                                           *dummy-sockaddr*
                                           *dummy-socklen*
                                           (logxor wsock:+SOCK-CLOEXEC+ wsock:+SOCK-NONBLOCK+))
                    #-linux (wsock:accept fd
                                          *dummy-sockaddr*
                                          *dummy-socklen*)))
    (case client-fd
      (-1 (let ((errno (wsys:errno)))
            (cond
              ((or (= errno wsys:EWOULDBLOCK)
                   (= errno wsys:ECONNABORTED)
                   (= errno wsys:EPROTO)
                   (= errno wsys:EINTR)))
              (t
               (vom:error "Can't accept connection (Code: ~D)" errno)))))
      (otherwise
       #-linux (set-fd-nonblock client-fd t)

       ;; In case the client disappeared before closing the socket,
       ;; a socket object remains in the data registry.
       ;; I need to check if OS is gonna reuse the file descriptor.
       (let ((existing-socket (deref-data-from-pointer client-fd)))
         (when existing-socket
           (close-socket existing-socket)))
       (let* ((remote-addr (wsock:inet-ntoa
                            (cffi:foreign-slot-value *dummy-sockaddr* '(:struct wsock::sockaddr-in) 'wsock::addr)))
              (remote-port (cffi:foreign-slot-value *dummy-sockaddr* '(:struct wsock::sockaddr-in) 'wsock::port))
              (socket (make-socket :fd client-fd :tcp-read-cb 'tcp-read-cb
                        :remote-addr remote-addr :remote-port remote-port)))
         (setf (deref-data-from-pointer client-fd) socket)
         (let* ((callbacks (callbacks fd))
                (read-cb (getf callbacks :read-cb))
                (connect-cb (getf callbacks :connect-cb)))
           (when connect-cb
             (funcall (the function connect-cb) socket))
           (when read-cb
             (setf (socket-read-cb socket) read-cb)))
         (lev:ev-io-start evloop (socket-read-watcher socket))
         (let ((timer (socket-timeout-timer socket)))
           (lev:ev-timer-init timer 'timeout-cb *connection-timeout* 0.0d0)
           (setf (cffi:foreign-slot-value timer '(:struct lev:ev-timer) 'lev::data) (socket-read-watcher socket))
           (timeout-cb evloop timer lev:+EV-TIMER+)))))))

(defun vector-to-integer (vector)
  "Convert a vector to a 32-bit unsigned integer."
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

(defun address-to-vector (address)
  (map '(simple-array (unsigned-byte 8) (4))
       #'read-from-string
       (split-sequence #\. address)))

(defun listen-on (address port &key (backlog 128))
  (cffi:with-foreign-object (sin '(:struct wsock:sockaddr-in))
    (wsys:bzero sin (cffi:foreign-type-size '(:struct wsock:sockaddr-in)))
    (cffi:with-foreign-slots ((wsock::family wsock::addr wsock::port) sin (:struct wsock:sockaddr-in))
      (setf wsock::family wsock:+AF-INET+
            wsock::addr (htonl (vector-to-integer (address-to-vector address)))
            wsock::port (htons (or port 0))))
    (let ((fd (wsock:socket wsock:+AF-INET+ wsock:+SOCK-STREAM+ 0)))
      (when (= fd -1)
        (error 'os-error
               :description "Cannot create listening socket"
               :code (wsys:errno)))
      (let ((res (wsys:set-fd-nonblock fd t)))
        (when (= res -1)
          (error 'os-error
                 :description "Cannot set fd nonblock"
                 :code (wsys:errno))))
      (cffi:with-foreign-object (on :int)
        (setf (cffi:mem-aref on :int) 1)
        (when (= (wsock:setsockopt fd wsock:+SOL-SOCKET+ wsock:+SO-REUSEADDR+ on (cffi:foreign-type-size :int)) -1)
          (error 'os-error
                 :description "Cannot set socket option"
                 :code (wsys:errno))))
      (when (= (wsock:bind fd sin (cffi:foreign-type-size '(:struct wsock:sockaddr-in))) -1)
        (error 'os-error
               :description (format nil "Cannot bind fd to the address ~S" address)
               :code (wsys:errno)))
      (wsock:listen fd backlog)
      fd)))

(defun listen-on-fd (fd &key (backlog 128))
  (set-fd-nonblock fd t)
  (wsock:listen fd backlog)
  fd)

(defun make-listener (address port &key backlog fd)
  (let ((fd (if fd
                (listen-on-fd fd :backlog backlog)
                (listen-on address port :backlog backlog)))
        (listener (cffi:foreign-alloc '(:struct lev:ev-io))))
    (lev:ev-io-init listener 'tcp-accept-cb fd lev:+EV-READ+)
    listener))

(defun tcp-server (address port read-cb &key connect-cb (backlog 128) fd)
  (check-event-loop-running)
  (let ((listener (make-listener address port :backlog backlog :fd fd)))
    (lev:ev-io-start *evloop* listener)
    (setf (callbacks (io-fd listener)) (list :read-cb read-cb :connect-cb connect-cb))
    listener))

(defun close-tcp-server (watcher)
  (when watcher
    (let ((fd (io-fd watcher)))
      (when fd
        (wsys:close fd)))
    (cffi:foreign-free watcher)))
