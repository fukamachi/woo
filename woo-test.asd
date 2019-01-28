(defsystem "woo-test"
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("woo"
               "clack-test")
  :components
  ((:test-file "t/woo")
   (:test-file "t/ipv6"))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
