(in-package :cl-user)
(defpackage woo.ev.socket
  (:use :cl)
  (:import-from :woo.ev.event-loop
                :remove-callbacks)
  (:import-from :woo.ev.util
                :io-fd)
  (:import-from :iolib.syscalls
                #+nil :close)
  (:import-from :iolib.sockets
                :%sendto
                :msg-dontwait)
  (:import-from :cffi
                :with-pointer-to-vector-data
                :incf-pointer
                :foreign-free
                :null-pointer)
  (:export :socket
           :make-socket
           :socket-watcher
           :socket-data
           :socket-closed-p

           :write-socket-data
           :close-socket))
(in-package :woo.ev.socket)

(defstruct socket
  watcher
  data
  closed-p)

(defun close-watcher (watcher)
  (isys:close (io-fd watcher))
  (cffi:foreign-free watcher)
  (remove-callbacks watcher))

(defun close-socket (socket)
  (let ((watcher (socket-watcher socket)))
    (close-watcher watcher))
  (setf (socket-closed-p socket) t))

;; TODO: buffering when writing failed.
(defun write-socket-data (socket data &key (start 0) (end (length data))
                                        write-cb)
  (when (socket-closed-p socket)
    (error 'socket-closed))
  (let* ((watcher (socket-watcher socket))
         (fd (io-fd watcher)))
    (cffi:with-pointer-to-vector-data (data-sap data)
      (cffi:incf-pointer data-sap start)
      (let* ((nwrote 0)
             (len (- end start))
             (n (%sendto fd data-sap len sockets::msg-dontwait
                         (cffi:null-pointer) 0)))
        (loop
          (cond
            ((= n -1)
             (vom:error "Error while writing data: ~D" (isys:errno)))
            ((/= (+ nwrote n) len)
             (incf nwrote n)
             (cffi:incf-pointer data-sap n))
            (T (return))))
        (when write-cb
          (funcall (the function write-cb) socket))))))
