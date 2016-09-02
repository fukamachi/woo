(in-package :woo.llsocket)

(cffi:defcfun ("accept" accept) :int
  (socket :int)
  (address :pointer)  ;; sockaddr
  (addrlen :pointer))

#+linux
(cffi:defcfun ("accept4" accept4) :int
  (socket :int)
  (address :pointer)  ;; sockaddr
  (addrlen :pointer)
  (flags :int))

(cffi:defcfun ("bind" bind) :int
  (socket :int)
  (address :pointer)  ;; sockaddr
  (addrlen socklen-t))

(cffi:defcfun ("connect" connect) :int
  (socket :int)
  (address :pointer)  ;; sockaddr
  (addrlen socklen-t))

(cffi:defcfun ("getpeername" getpeername) :int
  (socket :int)
  (address :pointer)  ;; sockaddr
  (addrlen socklen-t))

(cffi:defcfun ("getsockname" getsockname) :int
  (socket :int)
  (address :pointer)  ;; sockaddr
  (addrlen socklen-t))

(cffi:defcfun ("getsockopt" getsockopt) :int
  (socket :int)
  (level :int)
  (optname :int)
  (optval :pointer)
  (optlen :int))

(cffi:defcfun ("inet_ntoa" inet-ntoa) :string
  (addr :int64))

(cffi:defcfun ("inet_ntop" inet-ntop) :string
  (af :int)
  (src :pointer)
  (dst :string)
  (size socklen-t))

(cffi:defcfun ("listen" listen) :int
  (socket :int)
  (backlog :int))

(cffi:defcfun ("recvfrom" recvfrom) ssize-t
  (socket :int)
  (buffer :pointer)
  (length size-t)
  (flags :int)
  (address :pointer)  ;; sockaddr
  (addrlen socklen-t))

(cffi:defcfun ("recvmsg" recvmsg) ssize-t
  (socket :int)
  (message :pointer)
  (flags :int))

(cffi:defcfun ("sendto" sendto) ssize-t
  (socket :int)
  (buffer :pointer)
  (length size-t)
  (flags :int)
  (destaddr :pointer)  ;; sockaddr
  (destlen socklen-t))

(cffi:defcfun ("sendmsg" sendmsg) ssize-t
  (socket :int)
  (message :pointer)
  (flags :int))

(cffi:defcfun ("setsockopt" setsockopt) :int
  (socket :int)
  (level :int)
  (optname :int)
  (optval :pointer)
  (optlen socklen-t))

(cffi:defcfun ("shutdown" shutdown) :int
  (socket :int)
  (how :int))

(cffi:defcfun ("socket" socket) :int
  (domain :int)  ;; +AF-*+
  (type :int)    ;; +SOCK-*+
  (protocol :int))

(cffi:defcfun ("socketpair" socketpair) :int
  (domain :int)  ;; +AF-*+
  (type :int)    ;; +SOCK-*+
  (protocol :int)
  (fd :int))

(cffi:defcfun ("getaddrinfo" getaddrinfo) :int
  (node :string)
  (service :string)
  (hints (:pointer addrinfo))
  (res (:pointer (:pointer addrinfo))))

(cffi:defcfun ("freeaddrinfo" freeaddrinfo) :void
  (res (:pointer addrinfo)))
