(in-package :cl-user)
(defpackage woo.ev.socket
  (:use :cl)
  (:import-from :woo.ev.event-loop
                :*evloop*
                :deref-data-from-pointer
                :remove-pointer-from-registry)
  (:import-from :woo.ev.util
                :io-fd
                :define-c-callback)
  (:import-from :woo.ev.syscall
                #+nil :close
                #+nil :write
                :EWOULDBLOCK
                :EINTR
                :ECONNABORTED
                :ECONNREFUSED
                :ECONNRESET)
  (:import-from :iolib.sockets
                :%shutdown
                :shut-rdwr
                :socket-not-connected-error)
  (:import-from :ev
                :ev_now
                :ev_io
                :ev_io_init
                :ev_io_start
                :ev_io_stop
                :ev_timer
                :ev_timer_stop
                :EV_WRITE)
  (:import-from :fast-io
                :make-output-buffer
                :fast-write-sequence
                :fast-write-byte
                :finish-output-buffer)
  (:import-from :cffi
                :with-pointer-to-vector-data
                :incf-pointer
                :foreign-free)
  (:import-from :alexandria
                :ignore-some-conditions)
  (:export :socket
           :make-socket
           :socket-read-watcher
           :socket-write-watcher
           :socket-timeout-timer
           :socket-last-activity
           :socket-data
           :socket-read-cb
           :socket-open-p
           :check-socket-open

           :write-socket-data
           :write-socket-byte
           :flush-buffer
           :with-async-writing
           :close-socket))
(in-package :woo.ev.socket)

(defstruct (socket (:constructor %make-socket))
  (watchers (make-array 3
                        :element-type 'cffi:foreign-pointer
                        :initial-contents (list (cffi:foreign-alloc 'ev::ev_io)
                                                (cffi:foreign-alloc 'ev::ev_io)
                                                (cffi:foreign-alloc 'ev::ev_timer)))
   :type (simple-array cffi:foreign-pointer (3)))
  (last-activity (ev::ev_now *evloop*) :type double-float)
  (fd nil :type fixnum)
  data
  (tcp-read-cb nil :type symbol)
  (read-cb nil :type (or null function))
  (write-cb nil :type (or null function))
  (open-p t :type boolean)

  (buffer (make-output-buffer)))

(defun make-socket (&rest initargs &key tcp-read-cb fd &allow-other-keys)
  (let ((socket (apply #'%make-socket initargs)))
    (ev::ev_io_init (socket-read-watcher socket)
                    tcp-read-cb
                    fd
                    ev:EV_READ)
    socket))

(declaim (inline socket-read-watcher socket-write-watcher socket-timeout-timer))

(defun socket-read-watcher (socket)
  (svref (socket-watchers socket) 0))

(defun socket-write-watcher (socket)
  (svref (socket-watchers socket) 1))

(defun socket-timeout-timer (socket)
  (svref (socket-watchers socket) 2))

(defun close-socket (socket)
  (let ((read-watcher (socket-read-watcher socket))
        (write-watcher (socket-write-watcher socket))
        (timeout-timer (socket-timeout-timer socket)))
    (ev::ev_io_stop *evloop* read-watcher)
    (ev::ev_io_stop *evloop* write-watcher)
    (ev::ev_timer_stop *evloop* timeout-timer)
    (cffi:foreign-free read-watcher)
    (cffi:foreign-free write-watcher)
    (cffi:foreign-free timeout-timer))
  (let ((fd (socket-fd socket)))
    (ignore-some-conditions (socket-not-connected-error)
      (sockets::%shutdown fd sockets::shut-rdwr))
    (remove-pointer-from-registry fd))
  (setf (socket-open-p socket) nil
        (socket-read-cb socket) nil
        (socket-write-cb socket) nil
        (socket-buffer socket) nil
        (socket-data socket) nil)
  t)

(defun check-socket-open (socket)
  (unless (socket-open-p socket)
    (error 'socket-closed)))

(defun write-socket-data (socket data &key (start 0) (end (length data))
                                        write-cb)
  (declare (optimize speed)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (setf (socket-write-cb socket) write-cb)
  (fast-write-sequence data (socket-buffer socket)
                       start end))

(defun write-socket-byte (socket byte &key write-cb)
  (declare (optimize speed)
           (type (unsigned-byte 8) byte))
  (setf (socket-write-cb socket) write-cb)
  (fast-write-byte byte (socket-buffer socket)))

(declaim (inline reset-buffer))
(defun reset-buffer (socket)
  (let ((buffer (socket-buffer socket)))
    (setf (fast-io::output-buffer-vector buffer) (fast-io::make-octet-vector fast-io:*default-output-buffer-size*)
          (fast-io::output-buffer-fill buffer) 0
          (fast-io::output-buffer-len buffer) 0
          (fast-io::output-buffer-queue buffer) nil
          (fast-io::output-buffer-last buffer) nil)))

(defun flush-buffer (socket)
  (declare (optimize speed))
  (check-socket-open socket)
  (let ((data (finish-output-buffer (socket-buffer socket)))
        (fd (socket-fd socket))
        (write-cb (socket-write-cb socket)))
    (declare (type (simple-array (unsigned-byte 8) (*)) data))
    (cffi:with-pointer-to-vector-data (data-sap data)
      (let ((nwrote 0)
            (len (length data)))
        (declare (dynamic-extent nwrote))
        (loop
          (let ((n (wsys:write fd data-sap len)))
            (declare (dynamic-extent n))
            (case n
              (-1
               (let ((errno (isys:errno)))
                 (return-from flush-buffer
                   (cond
                     ((or (= errno wsys:EWOULDBLOCK)
                          (= errno wsys:EINTR))
                      nil)
                     ((or (= errno wsys:ECONNABORTED)
                          (= errno wsys:ECONNREFUSED)
                          (= errno wsys:ECONNRESET))
                      (vom:error "Connection is already closed (Code: ~D)" errno)
                      (close-socket socket)
                      t)
                     (t
                      (error "Unexpected error (Code: ~D)" errno))))))
              (otherwise
               (setf (socket-last-activity socket) (ev::ev_now *evloop*))
               (incf nwrote n)
               (if (= nwrote len)
                   (return)
                   (cffi:incf-pointer data-sap n))))))
        (when write-cb
          (funcall (the function write-cb) socket))
        ;; Need to check if 'socket' is still open because it may be closed in write-cb.
        (when (socket-open-p socket)
          (setf (socket-write-cb socket) nil)
          (reset-buffer socket))
        T))))

(define-c-callback async-write-cb :void ((evloop :pointer) (io :pointer) (events :int))
  (declare (ignore events))
  (let* ((fd (io-fd io))
         (socket (deref-data-from-pointer fd)))
    (unless socket
      (ev::ev_io_stop evloop io)
      (cffi:foreign-free io)
      (return-from async-write-cb))

    (let ((completedp (flush-buffer socket)))
      (when completedp
        (ev::ev_io_stop evloop io)))))

(defun init-write-io (socket &key write-cb)
  (setf (socket-write-cb socket) write-cb)
  (let ((io (socket-write-watcher socket))
        (fd (socket-fd socket)))
    (ev::ev_io_init io 'async-write-cb fd ev:EV_WRITE)
    (ev::ev_io_start *evloop* io)
    t))

(defmacro with-async-writing ((socket &key write-cb) &body body)
  `(progn
     ,@body
     (init-write-io ,socket :write-cb ,write-cb)))
