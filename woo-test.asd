(in-package :cl-user)
(defpackage woo-test-asd
  (:use :cl :asdf))
(in-package :woo-test-asd)

(defsystem woo-test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:woo
               :clack-test)
  :components
  ((:test-file "t/woo")
   (:test-file "t/ipv6"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
