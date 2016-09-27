(in-package :cl-user)
(defpackage woo.ev.util
  (:use :cl)
  (:import-from :lev
                :ev-io
                :fd)
  (:import-from :cffi
                :defcallback
                :foreign-slot-value)
  (:export :io-fd
           :define-c-callback))
(in-package :woo.ev.util)

(declaim (inline io-fd))
(defun io-fd (io)
  (cffi:foreign-slot-value io '(:struct lev:ev-io) 'lev::fd))

;; Copied from cl-async-util
;; Copyright (c) 2012 Lyon Bros. Enterprises, LLC
;; https://github.com/orthecreedence/cl-async/blob/master/LICENSE
(defmacro define-c-callback (name return-val (&rest args) &body body)
  "Define a top-level function with the given and also define a C callback that
   calls the function directly. The idea is that CFFI callbacks aren't directly
   callable/debuggable, but it's obnoxious to have to define and callback *and*
   a function right next to each other."
  (let ((arg-names (loop for x in args collect (car x))))
    `(progn
       (declaim (inline ,name))
       (defun ,name ,arg-names
         ,@body)
       (prog1
           (cffi:defcallback ,name ,return-val ,args
             (,name ,@arg-names))
         (declaim (notinline ,name))))))
