(in-package :cl-user)
(defpackage woo.llsocket
  (:nicknames :wsock)
  (:use :cl)
  (:shadow :listen)
  (:export :+AF-UNIX+
           :+AF-INET+
           :+AF-INET6+

           :+SOCK-STREAM+
           :+SOCK-DGRAM+
           :+SOCK-RAW+
           :+SOCK-RDM+
           :+SOCK-SEQPACKET+
           :+SOCK-CLOEXEC+
           :+SOCK-NONBLOCK+

           :+SO-DEBUG+
           :+SO-ACCEPTCONN+
           :+SO-REUSEADDR+
           :+SO-KEEPALIVE+
           :+SO-DONTROUTE+
           :+SO-BROADCAST+
           :+SO-USELOOPBACK+
           :+SO-LINGER+
           :+SO-OOBINLINE+

           :+SO-SNDBUF+
           :+SO-RCVBUF+
           :+SO-SNDLOWAT+
           :+SO-RCVTIMEO+
           :+SO-ERROR+
           :+SO-TYPE+

           :+SOMAXCONN+

           :+SOL-SOCKET+
           :+SOL-TCP+
           :+SOL-IP+
           :+SOL-RAW+

           :+IPPROTO-IP+
           :+IPPROTO-IPV6+
           :+IPPROTO-ICMP+
           :+IPPROTO-ICMPV6+
           :+IPPROTO-RAW+
           :+IPPROTO-TCP+
           :+IPPROTO-UDP+

           :+INADDR-ANY+
           :+INADDR-BROADCAST+
           :+INADDR-NONE+
           :+IN-LOOPBACKNET+
           :+INADDR-LOOPBACK+
           :+INADDR-UNSPEC-GROUP+
           :+INADDR-ALLHOSTS-GROUP+
           :+INADDR-ALLRTRS-GROUP+
           :+INADDR-MAX-LOCAL-GROUP+

           :+IP-HDRINCL+
           :+IP-RECVERR+

           :sockaddr
           :sockaddr-in

           :+MSG-OOB+
           :+MSG-PEEK+
           :+MSG-DONTROUTE+
           :+MSG-EOR+
           :+MSG-TRUNC+
           :+MSG-CTRUNC+
           :+MSG-WAITALL+

           :msghdr
           :cmsghdr

           :+SHUT-RD+
           :+SHUT-WR+
           :+SHUT-RDWR+

           :socklen-t

           :accept
           #+linux
           :accept4
           :bind
           :connect
           :getpeername
           :getsockname
           :getsockopt
           :inet-ntoa
           :listen
           :recvfrom
           :recvmsg
           :sendto
           :sendmsg
           :setsockopt
           :shutdown
           :socket
           :socketpair))
(in-package :woo.llsocket)
