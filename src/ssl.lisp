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

(defun init-ssl-handle (socket ssl-cert-file ssl-key-file ssl-key-password)
  (let ((client-fd (socket-fd socket)))
    (with-new-ssl (handle)
      (install-nonblock-flag client-fd)
      (ssl-set-fd handle client-fd)
      (ssl-set-accept-state handle)
      (when *default-cipher-list*
        (ssl-set-cipher-list handle *default-cipher-list*))
      (setf (socket-ssl-handle socket) handle)
      (with-pem-password ((or ssl-key-password ""))
        (install-key-and-cert
         handle
         ssl-key-file
         ssl-cert-file))
      socket)))
