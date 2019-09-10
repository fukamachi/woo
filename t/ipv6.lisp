(in-package :cl-user)
(defpackage woo-test.ipv6
  (:use :cl
        :rove)
  (:import-from :clack.test
                :testing-app
                :*clackup-additional-args*)
  (:import-from :clack.test.suite
                :localhost))
(in-package :woo-test.ipv6)

(let ((clack.test:*clackup-additional-args* '(:address "::"))
      (clack.test:*clack-test-handler* :woo))
  (deftest ipv6-tests
    (testing-app "IPv6"
        (lambda (env)
          (declare (ignore env))
          '(200 (:content-type "text/plain") ("Hello" "World")))
      (multiple-value-bind (body status)
        (dex:get (localhost))
        (ok (eql status 200))
        (ok (equal body "HelloWorld"))))))
