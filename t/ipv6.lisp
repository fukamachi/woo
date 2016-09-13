(in-package :cl-user)
(defpackage woo-test.ipv6
  (:use :cl
        :prove)
  (:import-from :clack.test
                :subtest-app
                :*clackup-additional-args*)
  (:import-from :clack.test.suite
                :localhost))
(in-package :woo-test.ipv6)

(plan 1)

(let ((clack.test:*clackup-additional-args* '(:address "::")))
  (subtest-app "IPv6"
      (lambda (env)
        (declare (ignore env))
        '(200 (:content-type "text/plain") ("Hello" "World")))
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (is status 200)
      (is body "HelloWorld"))))

(finalize)
