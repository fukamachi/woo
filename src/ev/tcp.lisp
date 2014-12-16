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
                :socket-read-watcher)
  (:import-from :woo.ev.util
                :define-c-callback
                :io-fd)
  (:import-from :ev
                :ev_io_init
                :ev_io_start
                :ev_io_stop
                :EV_READ)
  (:import-from :iolib.sockets
                #+nil :make-socket
                :make-address
                :bind-address
                :fd-of
                :%listen
                :%accept
                :with-sockaddr-storage-and-socklen
                :*default-backlog-size*)
  (:import-from :iolib.syscalls
                :%set-fd-nonblock
                #+nil :close
                #+nil :read
                :EWOULDBLOCK
                :ECONNABORTED
                :ECONNREFUSED
                :ECONNRESET
                :EPROTO
                :EINTR)
  (:import-from :cffi
                :foreign-alloc
                :foreign-free)
  (:import-from :alexandria
                :ignore-some-conditions)
  (:import-from :split-sequence
                :split-sequence)
  (:export :tcp-server
           :close-tcp-server))
(in-package :woo.ev.tcp)

(define-c-callback tcp-read-cb :void ((evloop :pointer) (watcher :pointer) (events :int))
  (declare (ignore evloop events))
  (let* ((fd (io-fd watcher))
         (buffer-len (length *input-buffer*))
         (socket (deref-data-from-pointer fd))
         (read-cb (socket-read-cb socket)))

    (handler-case
        (loop
          (let ((nread (isys:read fd (static-vectors:static-vector-pointer *input-buffer*) buffer-len)))
            (when (zerop nread)
              ;; EOF
              (close-socket socket)
              (return))

            (when read-cb
              (funcall (the function read-cb) socket *input-buffer* :start 0 :end nread))

            (unless (= nread buffer-len)
              (return))))
      ((or isys:EWOULDBLOCK isys:EINTR) ())
      ((or isys:ECONNABORTED isys:ECONNREFUSED isys:ECONNRESET) (e)
        (vom:error e)
        (close-socket socket)))))

(define-c-callback tcp-accept-cb :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore events))
  (ignore-some-conditions (isys:EWOULDBLOCK isys:ECONNABORTED isys:EPROTO isys:EINTR)
    (with-sockaddr-storage-and-socklen (sockaddr size)
      (let* ((fd (io-fd listener))
             (client-fd (%accept fd sockaddr size))
             (socket (make-socket :fd client-fd :tcp-read-cb 'tcp-read-cb)))
        (setf (deref-data-from-pointer client-fd) socket)
        (let* ((callbacks (callbacks fd))
               (read-cb (getf callbacks :read-cb))
               (connect-cb (getf callbacks :connect-cb)))
          (when connect-cb
            (funcall (the function connect-cb) socket))
          (when read-cb
            (setf (socket-read-cb socket) read-cb)))
        (ev::ev_io_start evloop (socket-read-watcher socket))))))

(defun listen-on (address port &key (backlog *default-backlog-size*))
  (let ((address (sockets:make-address
                  (map '(simple-array (unsigned-byte 8) (4))
                       #'read-from-string
                       (split-sequence #\. address))))
        (socket (sockets:make-socket :connect :passive
                                     :address-family :internet
                                     :ipv6 nil)))
    (%set-fd-nonblock (fd-of socket) t)
    (bind-address socket address :port port)
    (%listen (fd-of socket) backlog)
    (fd-of socket)))

(defun make-listener (address port &key backlog)
  (let ((fd (listen-on address port :backlog backlog))
        (listener (cffi:foreign-alloc 'ev::ev_io)))
    (ev::ev_io_init listener 'tcp-accept-cb fd ev:EV_READ)
    listener))

(defun tcp-server (address port read-cb &key connect-cb (backlog *default-backlog-size*))
  (check-event-loop-running)
  (let ((listener (make-listener address port :backlog backlog)))
    (ev::ev_io_start *evloop* listener)
    (setf (callbacks (io-fd listener)) (list :read-cb read-cb :connect-cb connect-cb))
    listener))

(defun close-tcp-server (watcher)
  (isys:close (io-fd watcher))
  (cffi:foreign-free watcher))
