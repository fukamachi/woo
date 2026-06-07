(defpackage woo.ssl
  (:use :cl)
  (:import-from :cl+ssl
                :make-context
                :with-global-context
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
  (:export :create-context
           :init-ssl-handle
           :free-ctx))
(in-package :woo.ssl)

(defun create-context (ssl-cert-file ssl-key-file ssl-key-password)
  (make-context :certificate-chain-file ssl-cert-file
                :private-key-file ssl-key-file
                :private-key-password ssl-key-password
                :verify-mode cl+ssl:+ssl-verify-none+))

(defun init-ssl-handle (socket ssl-ctx ssl-cert-file ssl-key-file ssl-key-password)
  (with-global-context (ssl-ctx)
    (let* ((client-fd (socket-fd socket)))
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
        socket))))

(defun free-ctx (ssl-ctx)
  (cl+ssl:ssl-ctx-free ssl-ctx))
