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
           :+SO-REUSEPORT+
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
           :sockaddr-storage
           :sockaddr-in
           :in6-addr
           :sockaddr-in6
           :sockaddr-un
           :addrinfo

           :+MSG-OOB+
           :+MSG-PEEK+
           :+MSG-DONTROUTE+
           :+MSG-EOR+
           :+MSG-TRUNC+
           :+MSG-CTRUNC+
           :+MSG-WAITALL+

           :+AI-PASSIVE+
           :+AI-CANONNAME+
           :+AI-NUMERICHOST+
           :+AI-V4MAPPED+
           :+AI-ALL+
           :+AI-ADDRCONFIG+

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
           :inet-ntop
           :listen
           :recvfrom
           :recvmsg
           :sendto
           :sendmsg
           :setsockopt
           :shutdown
           :socket
           :socketpair
           :getaddrinfo
           :freeaddrinfo

           :so-reuseport-available-p))
(in-package :woo.llsocket)

(defun so-reuseport-available-p ()
  #+linux
  (let ((kernel-version
          (with-output-to-string (s)
            (uiop:run-program "uname -r"
                              :output s
                              :ignore-error-status t))))
    (setq kernel-version
          (if (= 0 (length kernel-version))
              nil
              (subseq kernel-version 0 (1- (length kernel-version)))))
    (when kernel-version
      (multiple-value-bind (major read-count)
          (parse-integer kernel-version :junk-allowed t)
        (let ((minor (parse-integer kernel-version :start (1+ read-count) :junk-allowed t)))
          (and major minor
               (or (< 3 major)
                   (and (= 3 major)
                        (<= 9 minor))))))))
  #-linux nil)
