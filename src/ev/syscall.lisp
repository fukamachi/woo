(in-package :cl-user)
(defpackage woo.ev.syscall
  (:nicknames :wsys)
  (:use :cl)
  (:shadow :close
           :write
           :read)
  (:import-from :cffi
                :defcfun)
  (:export :close
           :write
           :read
           :accept
           :set-fd-nonblock
           :errno
           :EWOULDBLOCK
           :EINTR
           :EPROTO
           :ECONNABORTED
           :ECONNREFUSED
           :ECONNRESET))
(in-package :woo.ev.syscall)

(defcfun ("close") :int
  (fd :int))

(defcfun ("write") :unsigned-int
  (fd :int)
  (buf :pointer)
  (count :unsigned-int))

(defcfun ("read") :unsigned-int
  (fd :int)
  (buf :pointer)
  (count :unsigned-int))

(defcfun ("fcntl" %fcntl/noarg) :int
  (fd  :int)
  (cmd :int))

(defcfun ("fcntl" %fcntl/int) :int
  (fd  :int)
  (cmd :int)
  (arg :int))

(defconstant F-GETFL 3.)
(defconstant F-SETFL 4.)
(defconstant O-NONBLOCK 4.)

(defconstant EWOULDBLOCK 35.)
(defconstant EINTR 4.)
(defconstant EPROTO 100.)
(defconstant ECONNABORTED 53.)
(defconstant ECONNREFUSED 61.)
(defconstant ECONNRESET 54.)

(defun set-fd-nonblock (fd enabled)
  (let ((current-flags (%fcntl/noarg fd F-GETFL)))
    (if (< current-flags 0)
        -1
        (let ((new-flags
                (if enabled
                    (logxor current-flags O-NONBLOCK)
                    (logand current-flags (lognot O-NONBLOCK)))))
          (if (= new-flags current-flags)
              (%fcntl/int fd F-SETFL new-flags)
              0)))))

(defcfun (errno "errno") :int)

(defcfun ("accept") :int
  (socket :int)
  (address :pointer)
  (addrlen :pointer))
