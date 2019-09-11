(defsystem "woo"
  :version "0.12.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("lev"
               "clack-socket"
               "swap-bytes"
               "cffi"
               "static-vectors"
               "bordeaux-threads"
               "fast-http"
               "quri"
               "fast-io"
               "smart-buffer"
               "trivial-utf-8"
               "vom"
               "alexandria"
               #+sbcl "sb-posix"
               #+(and linux (not asdf3)) "uiop"
               #+sbcl "sb-concurrency"
               #-sbcl "cl-speedy-queue")
  :components ((:module "src"
                :components
                ((:file "woo" :depends-on ("ev" "response" "worker" "signal" "specials" "util"))
                 (:file "response" :depends-on ("ev"))
                 (:file "ev" :depends-on ("ev-packages"))
                 (:file "worker" :depends-on ("ev" "queue" "specials"))
                 (:file "queue")
                 (:module "ev-packages"
                  :pathname "ev"
                  :depends-on ("syscall" "llsocket")
                  :components
                  ((:file "event-loop")
                   (:file "socket" :depends-on ("event-loop" "condition" "util"))
                   (:file "tcp" :depends-on ("event-loop" "socket" "util" "condition"))
                   (:file "condition")
                   (:file "util")))
                 (:module "llsocket"
                  :depends-on ("syscall")
                  :serial t
                  :components
                  ((:file "package")
                   (:cffi-grovel-file "grovel")
                   (:file "cffi")))
                 (:module "syscall"
                  :serial t
                  :components
                  ((:file "package")
                   (:cffi-grovel-file "types")
                   (:file "main")))
                 (:file "signal" :depends-on ("ev" "worker" "specials"))
                 (:file "specials")
                 (:file "util"))))
  :description "An asynchronous HTTP server written in Common Lisp"
  :in-order-to ((test-op (test-op "woo-test"))))
