(in-package :cl-user)
(defpackage clack-handler-woo-asd
  (:use :cl :asdf))
(in-package :clack-handler-woo-asd)

(defsystem clack-handler-woo
  :depends-on (:woo))
