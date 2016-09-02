(in-package :cl-user)
(defpackage woo-test.ipv6
  (:use :cl))
(in-package :woo-test.ipv6)

(let ((clack.test:*clackup-additional-args* '(:address "::")))
  (clack.test.suite:run-server-tests :woo))
