(in-package :cl-user)
(defpackage woo.specials
  (:use :cl)
  (:export :*app*
           :*debug*
           :*ssl-context*
           :*listener*
           :*cluster*
           :default-thread-bindings))
(in-package :woo.specials)

(defvar *app* nil)
(defvar *debug* nil)
(defvar *ssl-context* nil)
(defun default-thread-bindings ()
  `((*standard-output* . ,*standard-output*)
    (*error-output* . ,*error-output*)
    (*app* . ,*app*)
    (*debug* . ,*debug*)
    (*ssl-context* . ,*ssl-context*)))

(defvar *listener* nil)
(defvar *cluster* nil)
