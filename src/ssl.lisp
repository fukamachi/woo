(defpackage woo.ssl
  (:use :cl)
  (:import-from :cl+ssl
                :with-new-ssl
                :install-nonblock-flag
                :ssl-set-fd
                :ssl-set-accept-state
                :*default-cipher-list*
                :ssl-set-cipher-list
                :with-pem-password
                :install-key-and-cert)
  (:import-from :woo.ev.socket
                :socket-fd
                :socket-ssl-handle)
  (:export :init-ssl-handle))
(in-package :woo.ssl)

;; SSL_set_mode is a C macro over SSL_ctrl, and cl+ssl exposes neither it nor
;; these constants. Values are from openssl/ssl.h.
(defconstant +ssl-ctrl-mode+ 33)
(defconstant +ssl-mode-enable-partial-write+ #x00000001)
(defconstant +ssl-mode-accept-moving-write-buffer+ #x00000002)

(defun ssl-set-mode (handle mode)
  (cl+ssl::ssl-ctrl handle +ssl-ctrl-mode+ mode (cffi:null-pointer)))

(defun init-ssl-handle (socket ssl-cert-file ssl-key-file ssl-key-password)
  (let ((client-fd (socket-fd socket)))
    (with-new-ssl (handle)
      (install-nonblock-flag client-fd)
      (ssl-set-fd handle client-fd)
      (ssl-set-accept-state handle)
      ;; The socket is non-blocking, so SSL_write can fail to consume the whole
      ;; buffer whenever a response outgrows the socket send buffer. Without
      ;; these two modes that case is unrecoverable:
      ;;
      ;; - ENABLE_PARTIAL_WRITE lets SSL_write report how much it accepted, so
      ;;   flush-buffer can re-queue the remainder the way the plain write()
      ;;   path already does. Otherwise SSL_write is all-or-nothing and just
      ;;   returns WANT_WRITE, which flush-buffer treats as a no-op.
      ;; - ACCEPT_MOVING_WRITE_BUFFER allows a retried SSL_write to pass the
      ;;   same contents at a different address. Woo re-pins its output buffer
      ;;   on every flush (finish-output-buffer + with-pointer-to-vector-data),
      ;;   so the pointer does move; OpenSSL otherwise fails the retry with a
      ;;   fatal SSL_ERROR_SSL and the connection dies mid-response.
      (ssl-set-mode handle (logior +ssl-mode-enable-partial-write+
                                   +ssl-mode-accept-moving-write-buffer+))
      (when *default-cipher-list*
        (ssl-set-cipher-list handle *default-cipher-list*))
      (setf (socket-ssl-handle socket) handle)
      (with-pem-password ((or ssl-key-password ""))
        (install-key-and-cert
         handle
         ssl-key-file
         ssl-cert-file))
      socket)))
