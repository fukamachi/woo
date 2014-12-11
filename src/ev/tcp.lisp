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
                :socket-closed-p)
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
                :%%receive-from
                :msg-dontwait
                :with-sockaddr-storage-and-socklen
                :*default-backlog-size*)
  (:import-from :iolib.syscalls
                :%set-fd-nonblock
                #+nil :close
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
  (declare (ignore events))
  (let* ((fd (io-fd watcher))
         (buffer-len (length *input-buffer*))
         (socket (deref-data-from-pointer fd))
         (read-cb (socket-read-cb socket)))
    (when (socket-closed-p socket)
      (return-from tcp-read-cb))

    (handler-case
        (loop
          (let ((nread (isys:read fd (static-vectors:static-vector-pointer *input-buffer*) buffer-len)))
            (when (zerop nread)
              ;; EOF: drain remaining writes or close connection
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

(defun make-client-watcher (client-fd)
  (%set-fd-nonblock client-fd t)
  (let ((io (cffi:foreign-alloc 'ev::ev_io)))
    (ev::ev_io_init io 'tcp-read-cb client-fd ev:EV_READ)
    io))

(define-c-callback tcp-accept-cb :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore events))
  (ignore-some-conditions (isys:EWOULDBLOCK isys:ECONNABORTED isys:EPROTO isys:EINTR)
    (with-sockaddr-storage-and-socklen (sockaddr size)
      (let* ((fd (io-fd listener))
             (client-fd (%accept fd sockaddr size))
             (client-watcher (make-client-watcher client-fd))
             (client-fd (io-fd client-watcher))
             (socket (make-socket :watcher client-watcher)))
        (setf (deref-data-from-pointer client-fd) socket)
        (prog1 (ev::ev_io_start evloop client-watcher)
          (let* ((callbacks (callbacks fd))
                 (read-cb (getf callbacks :read-cb))
                 (connect-cb (getf callbacks :connect-cb)))
            (when connect-cb
              (funcall connect-cb socket))
            (when read-cb
              (setf (socket-read-cb socket) read-cb))))))))

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
