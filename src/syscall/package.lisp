(in-package :cl-user)
(defpackage woo.syscall
  (:nicknames :wsys)
  (:use :cl)
  (:shadow :close
           :write
           :read
           :open)
  (:import-from :cffi
                :defcfun)
  (:export :close
           :write
           :read
           :kill
           :chmod
           :set-fd-nonblock
           :EWOULDBLOCK
           :EPIPE
           :EINTR
           :EPROTO
           :ECONNABORTED
           :ECONNREFUSED
           :ECONNRESET
           :ENOTCONN
           :EAGAIN

           :fork
           :memset
           :bzero

           :errno

           :getpid
           :getppid

           :sendfile
           :open))
(in-package :woo.syscall)
