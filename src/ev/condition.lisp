(in-package :cl-user)
(defpackage woo.ev.condition
  (:use :cl)
  (:export :woo-error
           :tcp-error
           :socket-closed
           :os-error))
(in-package :woo.ev.condition)

(define-condition woo-error (error)
  ((description :initarg :description)
   (code :initarg :code
         :initform nil))
  (:report (lambda (condition stream)
             (with-slots (description code) condition
               (format stream
                       "~A~:[~;~:* (Code: ~A)~]"
                       description code)))))

(define-condition tcp-error (woo-error) ())
(define-condition socket-closed (tcp-error)
  ((description :initform "socket is already closed")))

(define-condition os-error (woo-error) ())
