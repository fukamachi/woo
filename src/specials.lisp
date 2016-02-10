(in-package :cl-user)
(defpackage woo.specials
  (:use :cl)
  (:export :*app*
           :*debug*
           :*listener*
           :*cluster*
           :default-thread-bindings))
(in-package :woo.specials)

(defvar *app* nil)
(defvar *debug* nil)
(defun default-thread-bindings ()
  `((*standard-output* . ,*standard-output*)
    (*error-output* . ,*error-output*)
    (*app* . ,*app*)
    (*debug* . ,*debug*)))

(defvar *listener* nil)
(defvar *cluster* nil)
