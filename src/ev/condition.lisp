(in-package :cl-user)
(defpackage woo.ev.condition
  (:use :cl)
  (:export :tcp-error
           :socket-closed))
(in-package :woo.ev.condition)

(define-condition tcp-error (error) ())
(define-condition socket-closed (tcp-error) ())
