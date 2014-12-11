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
  (:import-from :iolib.syscalls
                #+nil :close
                #+nil write)
  (:import-from :ev
                :ev_io_init
                :ev_io_start
                :ev_io_stop
                :EV_WRITE)
  (:import-from :cffi
                :with-pointer-to-vector-data
                :incf-pointer
                :foreign-free
                :null-pointer)
  (:export :socket
           :make-socket
           :socket-watcher
           :socket-data
           :socket-read-cb
           :socket-closed-p
           :check-socket-open

           :write-socket-data
           :write-socket-data-async
           :close-socket))
(in-package :woo.ev.socket)

(defstruct socket
  watcher
  data
  read-cb
  write-cb
  closed-p

  buffer
  buffer-start
  buffer-end)

(declaim (inline close-watcher))

(defun close-watcher (watcher)
  (when *evloop*
    (ev::ev_io_stop *evloop* watcher))
  (let ((fd (io-fd watcher)))
    (isys:close fd)
    (remove-pointer-from-registry fd))
  (cffi:foreign-free watcher)
  t)

(defun close-socket (socket)
  (let ((watcher (socket-watcher socket)))
    (close-watcher watcher))
  (setf (socket-closed-p socket) t
        (socket-read-cb socket) nil
        (socket-write-cb socket) nil
        (socket-buffer socket) nil))

(declaim (notinline close-watcher))

(defun check-socket-open (socket)
  (when (socket-closed-p socket)
    (error 'socket-closed)))

;; TODO: buffering when writing failed.
(defun write-socket-data (socket data &key (start 0) (end (length data))
                                        write-cb)
  (check-socket-open socket)
  (let ((fd (io-fd (socket-watcher socket))))
    (cffi:with-pointer-to-vector-data (data-sap data)
      (cffi:incf-pointer data-sap start)
      (let* ((nwrote 0)
             (len (- end start)))
        (loop
          (let ((n (isys:write fd data-sap len)))
            (incf nwrote n)
            (if (= nwrote len)
                (return)
                (cffi:incf-pointer data-sap n))))
        (when write-cb
          (funcall (the function write-cb) socket))))))

(define-c-callback async-write-cb :void ((evloop :pointer) (io :pointer) (events :int))
  (declare (ignore events))
  (let* ((fd (io-fd io))
         (socket (deref-data-from-pointer fd)))
    (when socket
      (write-socket-data socket (socket-buffer socket)
                         :start (socket-buffer-start socket)
                         :end (socket-buffer-end socket)
                         :write-cb (socket-write-cb socket))

      (setf (socket-write-cb socket) nil
            (socket-buffer socket) nil)
      (ev::ev_io_stop evloop io)
      (cffi:foreign-free io))))

(defun write-socket-data-async (socket data &key (start 0) (end (length data))
                                              write-cb)
  (check-socket-open socket)
  (assert (null (socket-buffer socket)))
  (setf (socket-buffer socket) data
        (socket-buffer-start socket) start
        (socket-buffer-end socket) end
        (socket-write-cb socket) write-cb)
  (let ((io (cffi:foreign-alloc 'ev::ev_io))
        (fd (io-fd (socket-watcher socket))))
    (ev::ev_io_init io 'async-write-cb fd ev:EV_WRITE)
    (ev::ev_io_start *evloop* io)
    t))
