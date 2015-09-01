(include "sys/types.h" "sys/fcntl.h")
#+freebsd
(include "sys/socket.h")

(in-package :woo.syscall)

(ctype size-t "size_t")
(ctype ssize-t "ssize_t")
(ctype pid-t "pid_t")
(ctype off-t "off_t")
(ctype mode-t "mode_t")

#+freebsd
(constant (+SF-MNOWAIT+ "SF_MNOWAIT"))

(constant (+O-RDONLY+ "O_RDONLY"))
