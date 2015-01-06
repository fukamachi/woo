(in-package :cl-user)
(defpackage woo-test
  (:use :cl))
(in-package :woo-test)

(clack.test.suite:run-server-tests :woo)
