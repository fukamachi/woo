(include "sys/socket.h")

(in-package :woo.ev.syscall)

(constant (sock-cloexec "SOCK_CLOEXEC"))
(constant (sock-nonblock "SOCK_NONBLOCK"))
