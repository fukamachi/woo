(in-package :cl-user)
(defpackage woo.ev.event-loop
  (:use :cl)
  (:import-from :ev
                :ev_loop_new
                :ev_run)
  (:import-from :cffi
                :foreign-free)
  (:import-from :static-vectors
                :make-static-vector
                :free-static-vector)
  (:export :with-event-loop
           :check-event-loop-running

           :*evloop*
           :*buffer-size*
           :*input-buffer*

           :callbacks
           :remove-callbacks
           :deref-data-from-pointer
           :remove-pointer-from-registry))
(in-package :woo.ev.event-loop)

(defparameter *evloop* nil)
(defvar *buffer-size* (* 1024 64))
(defparameter *input-buffer* nil)

(defvar *callbacks* nil)
(defvar *data-registry* nil)

(defun (setf callbacks) (callbacks pointer)
  (setf (gethash pointer *callbacks*) callbacks))

(defun callbacks (pointer)
  (when *callbacks*
    (gethash pointer *callbacks*)))

(defun remove-callbacks (pointer)
  (when *callbacks*
    (remhash pointer *callbacks*)))

(defun deref-data-from-pointer (pointer)
  (when *data-registry*
    (gethash pointer *data-registry*)))

(defun (setf deref-data-from-pointer) (data pointer)
  (setf (gethash pointer *data-registry*) data))

(defun remove-pointer-from-registry (pointer)
  (when *data-registry*
    (remhash pointer *data-registry*)))

(defmacro with-event-loop (&body body)
  `(let ((*evloop* (ev::ev_loop_new 0))
         (*callbacks* (make-hash-table :test 'eql))
         (*data-registry* (make-hash-table :test 'eql))
         (*input-buffer* (make-static-vector *buffer-size*)))
     (unwind-protect (progn
                       ,@body
                       (ev::ev_run *evloop* 0))
       (free-static-vector *input-buffer*)
       (cffi:foreign-free *evloop*))))

(defun check-event-loop-running ()
  (unless *evloop*
    (error "Event loop not running")))
