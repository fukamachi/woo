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
  (:import-from :woo.syscall
                #+nil :close
                #+nil :write
                :errno
                :EWOULDBLOCK
                :EINTR
                :ECONNABORTED
                :ECONNREFUSED
                :ECONNRESET)
  (:import-from :woo.ev.condition
                :socket-closed)
  (:import-from :lev
                :ev-now
                :ev-io
                :ev-io-init
                :ev-io-start
                :ev-io-stop
                :ev-timer
                :ev-timer-stop
                :+EV-READ+
                :+EV-WRITE+)
  (:import-from :fast-io
                :make-output-buffer
                :fast-write-sequence
                :fast-write-byte
                :finish-output-buffer)
  (:import-from :cffi
                :with-pointer-to-vector-data
                :incf-pointer
                :foreign-free)
  (:export :socket
           :make-socket
           :socket-read-watcher
           :socket-write-watcher
           :socket-timeout-timer
           :socket-last-activity
           :socket-remote-addr
           :socket-remote-port
           :socket-data
           :socket-read-cb
           :socket-open-p
           :check-socket-open

           :write-socket-data
           :write-socket-byte
           :flush-buffer
           :with-async-writing
           :send-static-file
           :close-socket))
(in-package :woo.ev.socket)

(defstruct (socket (:constructor %make-socket))
  (watchers (make-array 3
                        :element-type 'cffi:foreign-pointer
                        :initial-contents (list (cffi:foreign-alloc '(:struct lev:ev-io))
                                                (cffi:foreign-alloc '(:struct lev:ev-io))
                                                (cffi:foreign-alloc '(:struct lev:ev-timer))))
   :type (simple-array cffi:foreign-pointer (3)))
  (last-activity (lev:ev-now *evloop*) :type double-float)
  (fd nil :type fixnum)
  remote-addr
  remote-port
  data
  (tcp-read-cb nil :type symbol)
  (read-cb nil :type (or null function))
  (write-cb nil :type (or null function))
  (open-p t :type boolean)

  (buffer (make-output-buffer))
  (sendfile-fd nil :type (or null fixnum))
  (sendfile-size nil :type (or null integer))
  (sendfile-offset 0 :type (or null integer)))

(defun buffer-empty-p (socket)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (= (the fixnum (fast-io::output-buffer-len (socket-buffer socket))) 0))

(defun make-socket (&rest initargs &key tcp-read-cb fd &allow-other-keys)
  (let ((socket (apply #'%make-socket initargs)))
    (lev:ev-io-init (socket-read-watcher socket)
                    tcp-read-cb
                    fd
                    lev:+EV-READ+)
    (lev:ev-io-init (socket-write-watcher socket)
                    'async-write-cb
                    fd
                    lev:+EV-WRITE+)
    socket))

(declaim (inline socket-read-watcher socket-write-watcher socket-timeout-timer))

(defun socket-read-watcher (socket)
  (svref (socket-watchers socket) 0))

(defun socket-write-watcher (socket)
  (svref (socket-watchers socket) 1))

(defun socket-timeout-timer (socket)
  (svref (socket-watchers socket) 2))

(defun free-watchers (socket)
  (let ((read-watcher (socket-read-watcher socket))
        (write-watcher (socket-write-watcher socket))
        (timeout-timer (socket-timeout-timer socket)))
    (lev:ev-io-stop *evloop* read-watcher)
    (lev:ev-io-stop *evloop* write-watcher)
    (lev:ev-timer-stop *evloop* timeout-timer)
    (cffi:foreign-free read-watcher)
    (cffi:foreign-free write-watcher)
    (cffi:foreign-free timeout-timer)))

(defun close-socket (socket)
  (when (socket-open-p socket)
    (setf (socket-open-p socket) nil)
    (free-watchers socket)
    (let ((fd (socket-fd socket)))
      (wsys:close fd)
      (remove-pointer-from-registry fd))
    (setf (socket-read-cb socket) nil
          (socket-write-cb socket) nil
          (socket-buffer socket) nil
          (socket-data socket) nil)
    (let ((sendfile-fd (socket-sendfile-fd socket)))
      (when sendfile-fd
        (wsys:close sendfile-fd)
        (setf (socket-sendfile-fd socket) nil))))
  t)

(defun check-socket-open (socket)
  (unless (socket-open-p socket)
    (error 'socket-closed)))

(defun write-socket-data (socket data &key (start 0) (end (length data))
                                        (write-cb nil write-cb-specified-p))
  (declare (optimize speed)
           (type vector data)
           (type fixnum start end))
  (when (socket-open-p socket)
    (when write-cb-specified-p
      (setf (socket-write-cb socket) write-cb))
    (if (typep data '(simple-array (unsigned-byte 8) (*)))
        (fast-write-sequence data
                             (socket-buffer socket)
                             start end)
        (loop for i from start upto (1- end)
              for byte of-type (unsigned-byte 8) = (aref data i)
              do (fast-write-byte byte (socket-buffer socket))))))

(defun write-socket-byte (socket byte &key (write-cb nil write-cb-specified-p))
  (declare (optimize speed)
           (type (unsigned-byte 8) byte))
  (when (socket-open-p socket)
    (when write-cb-specified-p
      (setf (socket-write-cb socket) write-cb))
    (fast-write-byte byte (socket-buffer socket))))

(declaim (inline reset-buffer))
(defun reset-buffer (socket)
  (let ((buffer (socket-buffer socket)))
    (when buffer
      (setf (fast-io::output-buffer-vector buffer) (fast-io::make-octet-vector fast-io:*default-output-buffer-size*)
            (fast-io::output-buffer-fill buffer) 0
            (fast-io::output-buffer-len buffer) 0
            (fast-io::output-buffer-queue buffer) nil
            (fast-io::output-buffer-last buffer) nil))))

(defun flush-buffer (socket)
  (declare (optimize speed))
  (check-socket-open socket)
  (let ((data (finish-output-buffer (socket-buffer socket)))
        (fd (socket-fd socket)))
    (declare (type (simple-array (unsigned-byte 8) (*)) data))
    (cffi:with-pointer-to-vector-data (data-sap data)
      (let* ((len (length data))
             (completedp nil)
             (n (wsys:write fd data-sap len)))
        (declare (type fixnum len)
                 (type fixnum n))
        (case n
          (-1
           (let ((errno (wsys:errno)))
             (return-from flush-buffer
               (cond
                 ((or (= errno wsys:EWOULDBLOCK)
                      (= errno wsys:EINTR))
                  nil)
                 ((or (= errno wsys:ECONNABORTED)
                      (= errno wsys:ECONNREFUSED)
                      (= errno wsys:ECONNRESET)
                      (= errno wsys:EPIPE)
                      (= errno wsys:ENOTCONN))
                  (vom:error "Connection is already closed (Code: ~D)" errno)
                  (close-socket socket)
                  t)
                 (t
                  (vom:error "Unexpected error (Code: ~D)" errno)
                  (close-socket socket)
                  t)))))
          (otherwise
           (setf (socket-last-activity socket) (lev:ev-now *evloop*))
           (if (= n len)
               (setq completedp t)
               (progn
                 (reset-buffer socket)
                 (fast-write-sequence data
                                      (socket-buffer socket)
                                      n)))))
        completedp))))

(defun send-file (socket)
  (declare (optimize speed))
  (let* ((infd (socket-sendfile-fd socket))
         (offset (socket-sendfile-offset socket))
         (n (wsys:sendfile infd (socket-fd socket) offset
                           (min (- (socket-sendfile-size socket) offset)
                                (* 1024 100)))))
    (declare (type fixnum n))
    (cond
      ((= n -1)
       (let ((errno (wsys:errno)))
         (declare (type fixnum errno))
         (return-from send-file
           (cond
             ((or (= errno wsys:EWOULDBLOCK)
                  (= errno wsys:EINTR))
              nil)
             ((or (= errno wsys:ECONNABORTED)
                  (= errno wsys:ECONNREFUSED)
                  (= errno wsys:ECONNRESET)
                  (= errno wsys:EPIPE)
                  (= errno wsys:ENOTCONN))
              (vom:error "Connection is already closed (Code: ~D)" errno)
              (close-socket socket)
              t)
             (t
              (vom:error "Unexpected error (Code: ~D)" errno)
              (close-socket socket)
              t)))))
      (t
       (setf (socket-last-activity socket) (lev:ev-now *evloop*))
       (let ((completedp (= (socket-sendfile-size socket)
                            (incf (socket-sendfile-offset socket) n))))
         (when completedp
           (wsys:close infd)
           (setf (socket-sendfile-fd socket) nil))
         completedp)))))

(defun async-write (socket)
  (declare (optimize speed))
  (unless (socket-open-p socket)
    (return-from async-write t))

  ;; Send from buffer
  (unless (buffer-empty-p socket)
    (unless (flush-buffer socket)
      (return-from async-write nil))
    (reset-buffer socket))
  ;; Send a static file?
  (when (socket-sendfile-fd socket)
    (unless (send-file socket)
      (return-from async-write nil)))

  ;; Transfer has been completed.
  (when (socket-write-cb socket)
    (funcall (the function (socket-write-cb socket)) socket))
  ;; Need to check if 'socket' is still open because it may be closed in write-cb.
  (when (socket-open-p socket)
    (setf (socket-write-cb socket) nil)
    (lev:ev-io-stop *evloop* (socket-write-watcher socket)))
  t)

(define-c-callback async-write-cb :void ((evloop :pointer) (io :pointer) (events :int))
  (declare (optimize speed)
           (ignore events))
  (let* ((fd (io-fd io))
         (socket (deref-data-from-pointer fd)))
    (unless socket
      (lev:ev-io-stop evloop io)
      (cffi:foreign-free io)
      (return-from async-write-cb))

    (async-write socket)))

(defmacro with-async-writing ((socket &key write-cb force-streaming) &body body)
  `(progn
     ,@body
     (setf (socket-write-cb ,socket) ,write-cb)
     ,(if force-streaming
          `(unless (async-write ,socket)
             (lev:ev-io-start *evloop* (socket-write-watcher ,socket)))
          `(lev:ev-io-start *evloop* (socket-write-watcher ,socket)))))

(defun send-static-file (socket fd size)
  (with-slots (sendfile-fd sendfile-size sendfile-offset) socket
    (when sendfile-fd
      (warn "Trying to send another file while sending a file.")
      (wsys:close sendfile-fd))
    (setf sendfile-fd fd
          sendfile-size size
          sendfile-offset 0)))
