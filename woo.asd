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

(defsystem woo
  :version "0.0.1"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:cl-async
               :fast-http
               :fast-io
               :chunga
               :babel
               :flexi-streams
               :bordeaux-threads
               :log4cl
               :lparallel
               :cffi
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "woo" :depends-on ("tcp" "url" "response"))
                 (:file "tcp")
                 (:file "response")
                 (:file "url"))))
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
          seq))))
