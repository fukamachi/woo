(in-package :cl-user)
(defpackage woo.llsocket
  (:nicknames :wsock)
  (:use :cl
        :split-sequence)
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
           :socketpair

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
      (destructuring-bind (major &optional minor)
          (split-sequence #\. kernel-version
                          :count 2)
        (let ((major (parse-integer major :junk-allowed t))
              (minor (and minor
                          (parse-integer minor :junk-allowed t))))
          (and major minor
               (or (< 3 major)
                   (and (= 3 major)
                        (<= 9 minor))))))))
  #-linux nil)
