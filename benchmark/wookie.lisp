;;
;; sbcl --load benchmark/wookie.lisp
;;
;; http://localhost:5000/

(ql:quickload :wookie)

(defpackage :wookie-test
    (:use :cl :wookie))
(in-package :wookie-test)

(defroute (:get "/") (req res)
  (send-response res :body "Hello, World"))

(as:with-event-loop (:catch-app-errors t)
  (start-server (make-instance 'listener :port 5000)))
