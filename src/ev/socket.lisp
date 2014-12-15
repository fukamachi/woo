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
           :socket-read-watcher
           :socket-write-watcher
           :socket-data
           :socket-read-cb
           :socket-open-p
           :check-socket-open

           :write-socket-data
           :write-socket-data-async
           :close-socket))
(in-package :woo.ev.socket)

(defstruct (socket (:constructor %make-socket))
  (watchers (make-array 2
                        :element-type 'cffi:foreign-pointer
                        :initial-contents (list (cffi:foreign-alloc 'ev::ev_io)
                                                (cffi:foreign-alloc 'ev::ev_io))))
  fd
  data
  tcp-read-cb
  read-cb
  write-cb
  (open-p t)

  buffer
  buffer-start
  buffer-end)

(defun make-socket (&rest initargs &key tcp-read-cb fd &allow-other-keys)
  (let ((socket (apply #'%make-socket initargs)))
    (ev::ev_io_init (socket-read-watcher socket)
                    tcp-read-cb
                    fd
                    ev:EV_READ)
    socket))

(defun socket-read-watcher (socket)
  (svref (socket-watchers socket) 0))

(defun socket-write-watcher (socket)
  (svref (socket-watchers socket) 1))

(defun close-socket (socket)
  (let ((read-watcher (socket-read-watcher socket))
        (write-watcher (socket-write-watcher socket)))
    (ev::ev_io_stop *evloop* read-watcher)
    (ev::ev_io_stop *evloop* write-watcher)
    (cffi:foreign-free read-watcher)
    (cffi:foreign-free write-watcher))
  (let ((fd (socket-fd socket)))
    (isys:close fd)
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

;; TODO: buffering when writing failed.
(defun write-socket-data (socket data &key (start 0) (end (length data))
                                        write-cb)
  (check-socket-open socket)
  (let ((fd (socket-fd socket)))
    (cffi:with-pointer-to-vector-data (data-sap data)
      (cffi:incf-pointer data-sap start)
      (let* ((nwrote 0)
             (len (- end start)))
        (handler-case
            (progn
              (loop
                (let ((n (isys:write fd data-sap len)))
                  (incf nwrote n)
                  (if (= nwrote len)
                      (return)
                      (cffi:incf-pointer data-sap n))))
              (when write-cb
                (funcall (the function write-cb) socket))
              (return-from write-socket-data t))
          ((or isys:EWOULDBLOCK isys:EINTR) ())
          ((or isys:ECONNABORTED isys:ECONNREFUSED isys:ECONNRESET) (e)
            (vom:error e)
            (close-socket socket)))))))

(define-c-callback async-write-cb :void ((evloop :pointer) (io :pointer) (events :int))
  (declare (ignore events))
  (let* ((fd (io-fd io))
         (socket (deref-data-from-pointer fd)))
    (unless socket
      (ev::ev_io_stop evloop io)
      (cffi:foreign-free io)
      (return-from async-write-cb))

    (let ((completedp
            (write-socket-data socket (socket-buffer socket)
                               :start (socket-buffer-start socket)
                               :end (socket-buffer-end socket)
                               :write-cb (socket-write-cb socket))))

      (when completedp
        (setf (socket-write-cb socket) nil
              (socket-buffer socket) nil)
        (ev::ev_io_stop evloop io)))))

(defun write-socket-data-async (socket data &key (start 0) (end (length data))
                                              write-cb)
  (check-socket-open socket)
  (assert (null (socket-buffer socket)))
  (setf (socket-buffer socket) data
        (socket-buffer-start socket) start
        (socket-buffer-end socket) end
        (socket-write-cb socket) write-cb)
  (let ((io (socket-write-watcher socket))
        (fd (socket-fd socket)))
    (ev::ev_io_init io 'async-write-cb fd ev:EV_WRITE)
    (ev::ev_io_start *evloop* io)
    t))
