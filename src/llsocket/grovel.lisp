#+freebsd
(progn
  (include "time.h")
  (include "sys/time.h"))

(include "sys/socket.h" "netinet/in.h" "netdb.h")

(in-package :woo.llsocket)

;; Address families
(constant (+AF-UNIX+ "AF_UNIX"))
(constant (+AF-INET+ "AF_INET"))
(constant (+AF-INET6+ "AF_INET6"))

;; Types
(constant (+SOCK-STREAM+ "SOCK_STREAM"))
(constant (+SOCK-DGRAM+ "SOCK_DGRAM"))
(constant (+SOCK-RAW+ "SOCK_RAW"))
(constant (+SOCK-RDM+ "SOCK_RDM"))
(constant (+SOCK-SEQPACKET+ "SOCK_SEQPACKET"))
(constant (+SOCK-CLOEXEC+ "SOCK_CLOEXEC") :optional t)
(constant (+SOCK-NONBLOCK+ "SOCK_NONBLOCK") :optional t)

;; Option flags per-socket
(constant (+SO-DEBUG+ "SO_DEBUG"))
(constant (+SO-ACCEPTCONN+ "SO_ACCEPTCONN"))
(constant (+SO-REUSEADDR+ "SO_REUSEADDR"))
(constant (+SO-REUSEPORT+ "SO_REUSEPORT"))
(constant (+SO-KEEPALIVE+ "SO_KEEPALIVE"))
(constant (+SO-DONTROUTE+ "SO_DONTROUTE"))
(constant (+SO-BROADCAST+ "SO_BROADCAST"))
(constant (+SO-USELOOPBACK+ "SO_USELOOPBACK"))
(constant (+SO-LINGER+ "SO_LINGER"))
(constant (+SO-OOBINLINE+ "SO_OOBINLINE"))

;; Additional options, not kept in so_options
(constant (+SO-SNDBUF+ "SO_SNDBUF"))
(constant (+SO-RCVBUF+ "SO_RCVBUF"))
(constant (+SO-SNDLOWAT+ "SO_SNDLOWAT"))
(constant (+SO-RCVLOWAT+ "SO_RCVLOWAT"))
(constant (+SO-SNDTIMEO+ "SO_SNDTIMEO"))
(constant (+SO-RCVTIMEO+ "SO_RCVTIMEO"))
(constant (+SO-ERROR+ "SO_ERROR"))
(constant (+SO-TYPE+ "SO_TYPE"))

;; Maximum queue length specifiable by listen
(constant (+SOMAXCONN+ "SOMAXCONN"))

;; Level number for (get/set)sockopt() to apply to socket itself
(constant (+SOL-SOCKET+ "SOL_SOCKET")
  :documentation "get/setsockopt socket level constant.")
(constant (+SOL-TCP+ "SOL_TCP")
  :documentation "get/setsockopt TCP level constant."
  :optional t)
(constant (+SOL-IP+ "SOL_IP")
  :documentation "get/setsockopt IP level constant."
  :optional t)
(constant (+SOL-RAW+ "SOL_RAW")
  :documentation "get/setsockopt raw level constant."
  :optional t)

(constant (+IPPROTO-IP+ "IPPROTO_IP"))
(constant (+IPPROTO-IPV6+ "IPPROTO_IPV6"))
(constant (+IPPROTO-ICMP+ "IPPROTO_ICMP"))
(constant (+IPPROTO-ICMPV6+ "IPPROTO_ICMPV6"))
(constant (+IPPROTO-RAW+ "IPPROTO_RAW"))
(constant (+IPPROTO-TCP+ "IPPROTO_TCP"))
(constant (+IPPROTO-UDP+ "IPPROTO_UDP"))

(constant (+INADDR-ANY+ "INADDR_ANY"))
(constant (+INADDR-BROADCAST+ "INADDR_BROADCAST"))
(constant (+INADDR-NONE+ "INADDR_NONE"))
(constant (+IN-LOOPBACKNET+ "IN_LOOPBACKNET"))
(constant (+INADDR-LOOPBACK+ "INADDR_LOOPBACK"))
(constant (+INADDR-UNSPEC-GROUP+ "INADDR_UNSPEC_GROUP"))
(constant (+INADDR-ALLHOSTS-GROUP+ "INADDR_ALLHOSTS_GROUP"))
(constant (+INADDR-ALLRTRS-GROUP+ "INADDR_ALLRTRS_GROUP"))
(constant (+INADDR-MAX-LOCAL-GROUP+ "INADDR_MAX_LOCAL_GROUP"))

;; IP options
(constant (+IP-HDRINCL+ "IP_HDRINCL"))
(constant (+IP-RECVERR+ "IP_RECVERR") :optional t)

;; addrinfo flags
(constant (+AI-PASSIVE+ "AI_PASSIVE"))
(constant (+AI-CANONNAME+ "AI_CANONNAME"))
(constant (+AI-NUMERICHOST+ "AI_NUMERICHOST"))
(constant (+AI-V4MAPPED+ "AI_V4MAPPED"))
(constant (+AI-ALL+ "AI_ALL"))
(constant (+AI-ADDRCONFIG+ "AI_ADDRCONFIG"))

;; POSIX types
(ctype size-t "size_t")
(ctype ssize-t "ssize_t")

;; Types (sys/socket.h)
(ctype socklen-t "socklen_t")
(ctype sa-family-t "sa_family_t")

;; Types (netinet/in.h)
(ctype sa-family-t "sa_family_t")
(ctype in-port-t "in_port_t")
(ctype in-addr-t "in_addr_t")

(cstruct sockaddr "struct sockaddr"
  (family "sa_family" :type sa-family-t))

(cstruct sockaddr-storage "struct sockaddr_storage"
  (family "ss_family" :type sa-family-t))

(cstruct sockaddr-in "struct sockaddr_in"
  (family "sin_family" :type sa-family-t)
  (port "sin_port" :type in-port-t)
  (addr "sin_addr" :type in-addr-t))

(cunion in6-addr "struct in6_addr"
  (addr8 "s6_addr" :type :uint8 :count :auto))

(cstruct sockaddr-in6 "struct sockaddr_in6"
  (family "sin6_family" :type sa-family-t)
  (port "sin6_port" :type in-port-t)
  (flowinfo "sin6_flowinfo" :type :uint32)
  (addr "sin6_addr" :type (:union in6-addr))
  (scope-id "sin6_scope_id" :type :uint32))

(cstruct addrinfo "struct addrinfo"
  (flags "ai_flags" :type :int)
  (family "ai_family" :type :int)
  (socktype "ai_socktype" :type :int)
  (protocol "ai_protocol" :type :int)
  (addrlen "ai_addrlen" :type socklen-t)
  (addr "ai_addr" :type :pointer)
  (canonname "ai_canonname" :type :string)
  (next "ai_next" :type :pointer))

(include "sys/un.h")

(cstruct sockaddr-un "struct sockaddr_un"
  (family "sun_family" :type sa-family-t)
  (path   "sun_path" :type :char))

;; Message headers
(constant (+MSG-OOB+ "MSG_OOB"))
(constant (+MSG-PEEK+ "MSG_PEEK"))
(constant (+MSG-DONTROUTE+ "MSG_DONTROUTE"))
(constant (+MSG-EOR+ "MSG_EOR"))
(constant (+MSG-TRUNC+ "MSG_TRUNC"))
(constant (+MSG-CTRUNC+ "MSG_CTRUNC"))
(constant (+MSG-WAITALL+ "MSG_WAITALL"))

(cstruct msghdr "struct msghdr"
  (name "msg_name" :type :pointer)
  (namelen "msg_namelen" :type socklen-t)
  (iov "msg_iov" :type :pointer)
  (iovlen "msg_iovlen" :type size-t)
  (control "msg_control" :type :pointer)
  (controllen "msg_controllen" :type socklen-t)
  (flags "msg_flags" :type :int))

(cstruct cmsghdr "struct cmsghdr"
  (len "cmsg_len" :type socklen-t)
  (level "cmsg_level" :type :int)
  (type "cmsg_type" :type :int))

(constant (+SHUT-RD+ "SHUT_RD"))
(constant (+SHUT-WR+ "SHUT_WR"))
(constant (+SHUT-RDWR+ "SHUT_RDWR"))
