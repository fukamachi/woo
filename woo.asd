#|
  This file is a part of woo project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage woo-asd
  (:use :cl :asdf))
(in-package :woo-asd)

#+quicklisp (ql:quickload :cffi-grovel)
#-quicklisp (asdf:load-system :cffi-grovel)

(defsystem woo
  :version "0.9.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:lev
               :clack-socket
               :swap-bytes
               :cffi
               :static-vectors
               :fast-http
               :quri
               :fast-io
               :trivial-utf-8
               :flexi-streams
               :vom
               :local-time
               :alexandria
               :split-sequence
               #+sbcl :sb-posix
               #+linux :uiop
               :sb-concurrency)
  :components ((:module "src"
                :components
                ((:file "woo" :depends-on ("ev" "response" "worker"))
                 (:file "response" :depends-on ("ev"))
                 (:file "ev" :depends-on ("ev-packages"))
                 (:file "worker" :depends-on ("ev"))
                 (:module "ev-packages"
                  :pathname "ev"
                  :depends-on ("syscall" "llsocket")
                  :components
                  ((:file "event-loop")
                   (:file "socket" :depends-on ("event-loop" "util"))
                   (:file "tcp" :depends-on ("event-loop" "socket" "util" "condition"))
                   (:file "condition")
                   (:file "util")))
                 (:module "llsocket"
                  :depends-on ("syscall")
                  :serial t
                  :components
                  ((:file "package")
                   (cffi-grovel:grovel-file "grovel")
                   (:file "cffi")))
                 (:module "syscall"
                  :serial t
                  :components
                  ((:file "package")
                   (cffi-grovel:grovel-file "types")
                   (:file "main"))))))
  :description "An asynchronous HTTP server written in Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op woo-test))))
