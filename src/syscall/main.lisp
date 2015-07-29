(in-package :woo.syscall)

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

(defcfun ("kill" kill) :int
  (pid :int)
  (sig :int))

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
  (declare (optimize (speed 3) (safety 0)))
  (let ((current-flags (%fcntl/noarg fd F-GETFL)))
    (declare (type fixnum current-flags))
    (if (< current-flags 0)
        -1
        (let ((new-flags
                (if enabled
                    (logxor current-flags O-NONBLOCK)
                    (logand current-flags (lognot O-NONBLOCK)))))
          (declare (type fixnum new-flags))
          (if (= new-flags current-flags)
              (%fcntl/int fd F-SETFL new-flags)
              0)))))

(defcfun (fork "fork") pid-t)

(defcfun (memset "memset") :pointer
  (buffer :pointer)
  (value :int)
  (count size-t))

(defun bzero (buffer count)
  (memset buffer 0 count))

;; errno(3) is not a C function in some environments (ex. Mac).
;; libfixposix can be a workaround for it, but I don't like to add a dependency on it
;; just for it.
(defun errno ()
  #+sbcl (sb-impl::get-errno)
  #+ccl (ccl::%get-errno)
  #-(or sbcl ccl) nil)

(defcfun (getpid "getpid") pid-t)

(defcfun (getppid "getppid") pid-t)
