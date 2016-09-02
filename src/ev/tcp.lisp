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
                :socket-fd
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
                :sockaddr-in6
                :sockaddr-storage
                :inet-ntoa
                :inet-ntop
                :setsockopt
                :addrinfo
                :getaddrinfo
                :freeaddrinfo
                :+AF-INET+
                :+AF-INET6+
                :+AI-PASSIVE+
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
                :foreign-slot-pointer
                :with-foreign-object
                :with-foreign-slots
                :mem-aref
                :mem-ref
                :null-pointer
                :foreign-type-size
                :foreign-string-to-lisp)
  (:export :tcp-server
           :close-tcp-server
           :with-sockaddr
           :start-listening-socket
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
        (declare (type fixnum n))
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
               ((= errno wsys:EAGAIN)
                ;; Just to nothing
                )
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

(defvar *dummy-sockaddr*)
(defvar *dummy-socklen*)
(defvar *dummy-sockstring*)

(defmacro with-sockaddr (&body body)
  `(let* ((*dummy-sockaddr* (cffi:foreign-alloc '(:struct wsock:sockaddr-storage)))
          (*dummy-socklen* (cffi:foreign-alloc 'wsock:socklen-t))
          (*dummy-sockstring* (cffi:foreign-alloc :char :count 46)))
     (wsys:bzero *dummy-sockaddr* (cffi:foreign-type-size '(:struct wsock:sockaddr-storage)))
     (setf (cffi:mem-aref *dummy-socklen* 'wsock:socklen-t) (cffi:foreign-type-size '(:struct wsock:sockaddr-storage)))
     (dotimes (i 46)
       (setf (mem-ref *dummy-sockstring* :char i) 0))
     (unwind-protect
          (progn ,@body)
       (cffi:foreign-free *dummy-sockaddr*)
       (cffi:foreign-free *dummy-socklen*)
       (cffi:foreign-free *dummy-sockstring*))))

(defun get-remote-addr-and-port ()
  (declare (optimize (speed 3) (safety 2) (debug 2)))
  (let ((family (cffi:foreign-slot-value *dummy-sockaddr* '(:struct wsock:sockaddr-storage) 'wsock::family)))
    (declare (type fixnum family))
    (cond
      ((= family wsock:+AF-INET6+)
       (wsock:inet-ntop
        family
        (cffi:foreign-slot-pointer *dummy-sockaddr* '(:struct wsock:sockaddr-in6) 'wsock::addr)
        *dummy-sockstring*
        (cffi:mem-aref *dummy-socklen* :int))
       (values
        (cffi:foreign-string-to-lisp *dummy-sockstring*)
        (cffi:foreign-slot-value *dummy-sockaddr* '(:struct wsock:sockaddr-in6) 'wsock::port)))
      ((= family wsock:+AF-INET+)
       (values
        (wsock:inet-ntoa
         (cffi:foreign-slot-value *dummy-sockaddr* '(:struct wsock::sockaddr-in) 'wsock::addr))
        (cffi:foreign-slot-value *dummy-sockaddr* '(:struct wsock:sockaddr-in) 'wsock::port)))
      (t (values nil nil)))))

(define-c-callback tcp-accept-cb :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore evloop events))
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
       (multiple-value-bind (remote-addr remote-port)
           (get-remote-addr-and-port)
         (let ((socket (make-socket :fd client-fd :tcp-read-cb 'tcp-read-cb
                                    :remote-addr remote-addr :remote-port remote-port)))
           (let* ((callbacks (callbacks fd))
                  (read-cb (getf callbacks :read-cb))
                  (connect-cb (getf callbacks :connect-cb)))
             (when read-cb
               (setf (socket-read-cb socket) read-cb))
             (when connect-cb
               (funcall (the function connect-cb) socket)))))))))

(defun start-listening-socket (socket)
  (setf (deref-data-from-pointer (socket-fd socket)) socket)
  (lev:ev-io-start *evloop* (socket-read-watcher socket))
  (let ((timer (socket-timeout-timer socket)))
    (lev:ev-timer-init timer 'timeout-cb *connection-timeout* 0.0d0)
    (setf (cffi:foreign-slot-value timer '(:struct lev:ev-timer) 'lev::data) (socket-read-watcher socket))
    (timeout-cb *evloop* timer lev:+EV-TIMER+)))

(defun listen-on (address port &key (backlog 128) sockopt)
  (cffi:with-foreign-object (ai '(:pointer (:struct wsock:addrinfo)))
    (cffi:with-foreign-object (hints '(:struct wsock:addrinfo))
      (wsys:bzero hints (cffi:foreign-type-size '(:struct wsock:addrinfo)))
      (cffi:with-foreign-slots ((wsock::family wsock::socktype wsock::flags) hints (:struct wsock:addrinfo))
        (setf wsock::family (if (and (stringp address)
                                     (quri.domain:ipv6-addr-p address))
                                wsock:+AF-INET6+
                                wsock:+AF-INET+)
              wsock::socktype wsock:+SOCK-STREAM+
              wsock::flags wsock:+AI-PASSIVE+))
      (let ((err (wsock:getaddrinfo (or address
                                        (cffi:null-pointer))
                                    (write-to-string port)
                                    hints ai)))
        (unless (= err 0)
          (error 'os-error
                 :description "getaddrinfo() failed"
                 :code err))))
    (let ((ai (cffi:mem-ref ai :pointer)))
      (cffi:with-foreign-slots ((wsock::family
                                 wsock::socktype
                                 wsock::protocol
                                 wsock::addr
                                 wsock::addrlen)
                                ai
                                (:struct wsock:addrinfo))
        (let ((fd (wsock:socket wsock::family wsock::socktype wsock::protocol)))
          (when (= fd -1)
            (error 'os-error
                   :description (format nil "Cannot create listening socket (family=~S / socktype=~S / protocol=~S)"
                                        wsock::family
                                        wsock::socktype
                                        wsock::protocol)
                   :code (wsys:errno)))
          (let ((res (wsys:set-fd-nonblock fd t)))
            (when (= res -1)
              (error 'os-error
                     :description "Cannot set fd nonblock"
                     :code (wsys:errno))))
          (cffi:with-foreign-object (on :int)
            (setf (cffi:mem-aref on :int) 1)
            (when (= (wsock:setsockopt fd wsock:+SOL-SOCKET+ sockopt on (cffi:foreign-type-size :int)) -1)
              (error 'os-error
                     :description "Cannot set socket option"
                     :code (wsys:errno))))
          (when (= (wsock:bind fd wsock::addr wsock::addrlen) -1)
            (error 'os-error
                   :description (format nil "Cannot bind fd to the address ~S" address)
                   :code (wsys:errno)))
          (wsock:listen fd backlog)

          (wsock:freeaddrinfo ai)

          fd)))))

(defun listen-on-fd (fd &key (backlog 128))
  (set-fd-nonblock fd t)
  (wsock:listen fd backlog)
  fd)

(defun listen-on-unix (path &key (backlog 128) sockopt)
  (let ((fd (wsock:socket wsock:+AF-UNIX+ wsock:+SOCK-STREAM+ 0)))
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
      (when (= (wsock:setsockopt fd wsock:+SOL-SOCKET+ sockopt on (cffi:foreign-type-size :int)) -1)
        (error 'os-error
               :description "Cannot set socket option"
               :code (wsys:errno))))
    (when (probe-file path)
      (delete-file path))
    (let ((path (namestring path)))
      ;; TODO: check if the path is too long
      (cffi:with-foreign-object (sun '(:struct wsock:sockaddr-un))
        (wsys:bzero sun (cffi:foreign-type-size '(:struct wsock:sockaddr-un)))
        (setf (cffi:foreign-slot-value sun '(:struct wsock:sockaddr-un) 'wsock::family)
              wsock:+AF-UNIX+)
        (let ((sun-name-ptr (cffi:foreign-slot-pointer sun '(:struct wsock:sockaddr-un) 'wsock::path)))
          (dotimes (i (length path))
            (setf (cffi:mem-aref sun-name-ptr :char i) (char-code (elt path i)))))
        (when (= (wsock:bind fd sun (+ (cffi:foreign-type-size 'wsock::sa-family-t)
                                       (length path)
                                       1))
                 -1)
          (error 'os-error
                 :description (format nil "Cannot bind fd to ~S" path)
                 :code (wsys:errno)))
        (wsys:chmod path #o777)))
    (wsock:listen fd backlog)
    fd))

(defun make-listener (address port &key backlog fd sockopt)
  (let ((fd (if fd
                (listen-on-fd fd :backlog backlog)
                (listen-on address port :backlog backlog :sockopt sockopt)))
        (listener (cffi:foreign-alloc '(:struct lev:ev-io))))
    (lev:ev-io-init listener 'tcp-accept-cb fd lev:+EV-READ+)
    listener))

(defun tcp-server (address-port read-cb &key connect-cb (backlog 128) fd (sockopt wsock:+SO-REUSEADDR+))
  (check-event-loop-running)
  (etypecase address-port
    (cons
     (let* ((address (car address-port))
            (port (cdr address-port))
            (listener (make-listener address port :backlog backlog :fd fd :sockopt sockopt)))
       (lev:ev-io-start *evloop* listener)
       (setf (callbacks (io-fd listener)) (list :read-cb read-cb :connect-cb connect-cb))
       listener))
    (pathname
     (let ((fd (listen-on-unix address-port :backlog backlog :sockopt sockopt))
           (listener (cffi:foreign-alloc '(:struct lev:ev-io))))
       (lev:ev-io-init listener 'tcp-accept-cb fd lev:+EV-READ+)
       (lev:ev-io-start *evloop* listener)
       (setf (callbacks (io-fd listener)) (list :read-cb read-cb :connect-cb connect-cb))
       listener))))

(defun close-tcp-server (watcher)
  (when watcher
    (let ((fd (io-fd watcher)))
      (when fd
        (wsys:close fd)))
    (cffi:foreign-free watcher)))
