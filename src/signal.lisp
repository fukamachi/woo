(in-package :cl-user)
(defpackage woo.signal
  (:use :cl
        :woo.specials)
  (:import-from :woo.worker
                :stop-cluster
                :kill-cluster)
  (:import-from :woo.ev
                :close-socket
                :*data-registry*)
  (:export :make-signal-watchers
           :start-signal-watchers
           :stop-signal-watchers))
(in-package :woo.signal)

(defvar *signals*
  '((2 . sigint-cb)
    (3 . sigquit-cb)
    (15 . sigint-cb)))

(cffi:defcallback sigquit-cb :void ((evloop :pointer) (signal :pointer) (events :int))
  (declare (ignore signal events))
  (vom:info "Terminating quiet workers...")
  (lev:ev-io-stop evloop *listener*)
  (if *cluster*
      (woo.worker:stop-cluster *cluster*)
      ;; Close existing all sockets for singlethreaded process.
      (maphash (lambda (fd socket)
                 (declare (ignore fd))
                 (wev:close-socket socket))
               wev:*data-registry*))
  (lev:ev-break evloop lev:+EVBREAK-ALL+))

(cffi:defcallback sigint-cb :void ((evloop :pointer) (signal :pointer) (events :int))
  (declare (ignore signal events))
  (vom:info "Terminating workers immediately...")
  (lev:ev-io-stop evloop *listener*)
  (if *cluster*
      (woo.worker:kill-cluster *cluster*)
      (maphash (lambda (fd socket)
                 (declare (ignore fd))
                 (wev:close-socket socket))
               wev:*data-registry*))
  (lev:ev-break evloop lev:+EVBREAK-ALL+))

(defun make-signal-watchers ()
  (let* ((watcher-count (length *signals*))
         (watchers
           (make-array watcher-count)))
    (dotimes (i watcher-count watchers)
      (setf (aref watchers i) (cffi:foreign-alloc '(:struct lev:ev-signal))))))

(defun start-signal-watchers (evloop watchers)
  (loop for (sig . cb) in *signals*
        for i from 0
        do (lev:ev-signal-init (aref watchers i) cb sig)
           (lev:ev-signal-start evloop (aref watchers i))))

(defun stop-signal-watchers (evloop watchers)
  (map nil (lambda (watcher)
             (lev:ev-signal-stop evloop watcher)
             (cffi:foreign-free watcher))
       watchers))
