(in-package :cl-user)
(defpackage woo-test
  (:use :cl
        :rove))
(in-package :woo-test)

(deftest woo-server-tests
  (clack.test.suite:run-server-tests :woo))

(deftest woo-ssl-server-tests
  (let ((clack.test:*clackup-additional-args*
          '(:ssl-cert-file #P"t/certs/localhost.crt"
            :ssl-key-file #P"t/certs/localhost.key"))
        (dex:*not-verify-ssl* t)
        (clack.test:*use-https* t))
    (clack.test.suite:run-server-tests :woo)))
