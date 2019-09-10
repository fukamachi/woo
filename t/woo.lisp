(in-package :cl-user)
(defpackage woo-test
  (:use :cl
        :rove))
(in-package :woo-test)

(deftest woo-server-tests
  (clack.test.suite:run-server-tests :woo))
