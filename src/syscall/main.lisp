(in-package :woo.syscall)

(defcfun (%open "open") :int
  (path :string)
  (flags :int)
  (mode mode-t))

(defun open (path &optional (flags +O-RDONLY+) (mode #o666))
  (check-type mode fixnum)
  (%open (cffi-sys:native-namestring (translate-logical-pathname path)) flags mode))

(defcfun ("close") :int
  (fd :int))

(defcfun ("write") ssize-t
  (fd :int)
  (buf :pointer)
  (count size-t))

(defcfun ("read") ssize-t
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

(defcfun ("chmod" chmod) :int
  (path :string)
  (mode mode-t))

(defconstant F-GETFL 3.)
(defconstant F-SETFL 4.)
(defconstant O-NONBLOCK 4.)

(defconstant EWOULDBLOCK 35.)
(defconstant EPIPE 32.)
(defconstant EINTR 4.)
(defconstant EPROTO 100.)
(defconstant ECONNABORTED 53.)
(defconstant ECONNREFUSED 61.)
(defconstant ECONNRESET 54.)
(defconstant ENOTCONN 57.)
(defconstant EAGAIN 11.)

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
#+(or sbcl ccl)
(declaim (ftype (function () fixnum) errno))
(defun errno ()
  #+sbcl (sb-impl::get-errno)
  #+ccl (ccl::%get-errno)
  #-(or sbcl ccl) nil)

(defcfun (getpid "getpid") pid-t)

(defcfun (getppid "getppid") pid-t)

#+linux
(defcfun (%sendfile "sendfile") ssize-t
  (infd   :int)
  (outfd  :int)
  (offset :pointer)
  (nbytes size-t))
#+darwin
(defcfun (%sendfile "sendfile") ssize-t
  (outfd  :int)
  (infd   :int)
  (offset off-t)
  (len :pointer)
  (hdtr :pointer)
  (flags :int))
#+freebsd
(defcfun (%sendfile "sendfile") ssize-t
  (infd   :int)
  (outfd  :int)
  (offset off-t)
  (nbytes size-t)
  (hdtr :pointer)
  (sbytes :pointer)
  (flags :int))

(defun sendfile (infd outfd offset nbytes)
  #+linux
  (cffi:with-foreign-object (off 'off-t)
    (setf (cffi:mem-aref off 'off-t) offset)
    (%sendfile outfd infd off nbytes))
  #+darwin
  (cffi:with-foreign-object (len 'off-t)
    (setf (cffi:mem-aref len 'off-t) nbytes)
    (let ((retval (%sendfile infd outfd offset len (cffi:null-pointer) 0)))
      (declare (type fixnum retval))
      (if (= retval -1)
          -1
          (cffi:mem-aref len 'off-t))))
  #+freebsd
  (cffi:with-foreign-object (sbytes 'off-t)
    (let ((retval (%sendfile infd outfd offset nbytes (cffi:null-pointer) sbytes +SF-MNOWAIT+)))
      (declare (type fixnum retval))
      (if (= retval -1)
          -1
          (cffi:mem-aref sbytes 'off-t))))
  #-(or linux darwin freebsd)
  (error "sendfile is not supported"))
