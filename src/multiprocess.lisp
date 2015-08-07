(in-package :cl-user)
(defpackage woo.multiprocess
  (:use :cl)
  (:export :*listener*
           :child-worker-pids))
(in-package :woo.multiprocess)

(defparameter *listener* nil)
(defvar *child-worker-pids* '())

(defun child-worker-pids ()
  *child-worker-pids*)

(defun (setf child-worker-pids) (pids)
  (setf *child-worker-pids* pids))
