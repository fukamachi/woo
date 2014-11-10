(in-package :cl-user)
(defpackage woo
  (:nicknames :clack.handler.woo)
  (:use :cl)
  (:import-from :woo.response
                :*empty-chunk*
                :fast-write-crlf
                :response-headers-bytes
                :write-response-headers
                :start-chunked-response
                :finish-response)
  (:import-from :quri
                :uri
                :uri-path
                :uri-query)
  (:import-from :fast-http
                :make-http-request
                :make-parser
                :http-method
                :http-resource
                :http-headers
                :http-major-version
                :http-minor-version
                :parsing-error
                :fast-http-error)
  (:import-from :cl-async
                :socket-closed
                :write-socket-data
                :socket-data
                :close-socket
                :with-event-loop
                :tcp-server
                :close-tcp-server
                :tcp-info
                :tcp-error
                :tcp-eof
                :tcp-socket)
  (:import-from :fast-io
                :make-output-buffer
                :finish-output-buffer
                :with-fast-output
                :fast-write-sequence
                :fast-write-byte)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string
                :utf-8-byte-length)
  (:import-from :flexi-streams
                :make-in-memory-output-stream)
  (:import-from :bordeaux-threads
                :make-thread
                :destroy-thread
                :make-lock
                :acquire-lock
                :release-lock
                :threadp)
  (:import-from :alexandria
                :hash-table-plist
                :copy-stream
                :if-let)
  (:export :run
           :stop))
(in-package :woo)

(defvar *app* nil)
(defvar *debug* nil)

(defun run (app &key (debug t) (port 5000) (address "0.0.0.0")
                  (use-thread #+thread-support t
                              #-thread-support nil)
                  (worker-num nil))
  (let ((server-started-lock (bt:make-lock "server-started"))
        (*app* app)
        (*debug* debug))
    (flet ((start-server ()
             (as:with-event-loop (:catch-app-errors t)
               (as:tcp-server address port
                              #'read-cb
                              #'event-cb
                              :connect-cb #'connect-cb)
               (bt:release-lock server-started-lock)))
           #-windows
           (start-server-multi ()
             (as:with-event-loop (:catch-app-errors t)
               (as:tcp-server address port
                              #'read-cb
                              #'event-cb
                              :connect-cb #'connect-cb)
               (let ((times worker-num))
                 (tagbody forking
                    (let ((pid #+sbcl (sb-posix:fork)
                               #-sbcl (osicat-posix:fork)))
                      (if (zerop pid)
                          (progn
                            (le:event-reinit (as::event-base-c as::*event-base*))
                            (unless (zerop (decf times))
                              (go forking)))
                          (progn
                            (le:event-reinit (as::event-base-c as::*event-base*))
                            (format t "Worker started: ~A~%" pid))))))
               (bt:release-lock server-started-lock))))
      (prog1 (let ((start-fn #-windows
                             (if worker-num
                                 #'start-server-multi
                                 #'start-server)
                             #+windows #'start-server)
                   (bt:*default-special-bindings* `((*app* . ,*app*)
                                                    (*debug* . ,*debug*))))
               (if (and (null worker-num) use-thread)
                   (bt:make-thread start-fn)
                   (funcall start-fn)))
        (bt:acquire-lock server-started-lock t)
        (sleep 0.05)))))

(defun connect-cb (socket)
  (setup-parser socket))

(defun read-cb (socket data)
  (let ((parser (as:socket-data socket)))
    (handler-case (funcall parser data)
      (fast-http:parsing-error (e)
        (log:error "fast-http parsing error: ~A" e)
        (write-response-headers socket 400 ())
        (finish-response socket (princ-to-string e)))
      (fast-http:fast-http-error (e)
        (log:error "fast-http error: ~A" e)
        (write-response-headers socket 500 ())
        (finish-response socket #.(trivial-utf-8:string-to-utf-8-bytes "Internal Server Error"))))))

(defun event-cb (event)
  (typecase event
    (as:tcp-eof ())
    (as:tcp-error ()
     (log:error (princ-to-string event)))
    (as:tcp-info
     (log:info (princ-to-string event))
     (let ((socket (as:tcp-socket event)))
       (write-response-headers socket 500 ())
       (finish-response socket "Internal Server Error")))
    (T
     (log:info event))))

(define-condition woo-error (simple-error) ())
(define-condition invalid-http-version (woo-error) ())

(defun http-version-keyword (major minor)
  (unless (= major 1)
    (error 'invalid-http-version))

  (case minor
    (1 :HTTP/1.1)
    (0 :HTTP/1.0)
    (otherwise (error 'invalid-http-version))))

(defun setup-parser (socket)
  (let ((http (make-http-request))
        (body-buffer (fast-io::make-output-buffer)))
    (setf (as:socket-data socket)
          (make-parser http
                       :body-callback
                       (lambda (data start end)
                         (fast-write-sequence data body-buffer start end))
                       :finish-callback
                       (lambda ()
                         (let ((env (nconc (list :raw-body
                                                 (flex:make-in-memory-input-stream
                                                  (fast-io::finish-output-buffer body-buffer)))
                                           (handle-request http socket))))
                           (setq body-buffer (fast-io::make-output-buffer))
                           (handle-response http socket
                                            (if *debug*
                                                (funcall *app* env)
                                                (if-let (res (handler-case (funcall *app* env)
                                                               (error (error)
                                                                 (log:error error)
                                                                 nil)))
                                                  res
                                                  '(500 nil nil))))))))))

(defun stop (server)
  (if (bt:threadp server)
      (bt:destroy-thread server)
      (as:close-tcp-server server)))


;;
;; Handling requests

(defun parse-host-header (host)
  (let ((pos (position #\: host :from-end t)))
    (unless pos
      (return-from parse-host-header
        (values host nil)))

    (let ((port (subseq host (1+ pos))))
      (if (every #'digit-char-p port)
          (values (subseq host 0 pos)
                  (read-from-string port))
          (values host nil)))))

(defun handle-request (http socket)
  (let ((uri (quri:uri (http-resource http)))
        (host (gethash "host" (http-headers http)))
        (headers (http-headers http)))
    (multiple-value-bind (server-name server-port)
        (if host
            (parse-host-header host)
            (values nil nil))
      (list :request-method (http-method http)
            :script-name ""
            :server-name server-name
            :server-port (or server-port 80)
            :server-protocol (http-version-keyword (http-major-version http) (http-minor-version http))
            :path-info (quri:url-decode (uri-path uri))
            :query-string (uri-query uri)
            :url-scheme :http
            :request-uri (http-resource http)
            :clack.streaming t
            :clack.nonblocking t
            :clack.io socket
            :content-length (gethash "content-length" headers)
            :content-type (gethash "content-type" headers)
            :headers headers))))


;;
;; Handling responses

(defun handle-response (http socket clack-res)
  (handler-case
      (etypecase clack-res
        (list (handle-normal-response http socket clack-res))
        (function (funcall clack-res (lambda (clack-res)
                                       (handler-case
                                           (handle-normal-response http socket clack-res)
                                         (as:socket-closed ()))))))
    (as:tcp-error (e)
      (log:error e))
    (t (e)
      (log:error e))))

(defun handle-normal-response (http socket clack-res)
  (let ((no-body '#:no-body)
        (close (or (= (http-minor-version http) 0)
                   (string-equal (gethash "connection" (http-headers http)) "close"))))
    (destructuring-bind (status headers &optional (body no-body))
        clack-res
      (when (eq body no-body)
        (let* ((stream (start-chunked-response socket status headers))
               (default-close close))
          (return-from handle-normal-response
            (lambda (body &key (close nil close-specified-p))
              (etypecase body
                (string (write-sequence (trivial-utf-8:string-to-utf-8-bytes body) stream))
                (vector (write-sequence body stream)))
              (force-output stream)
              (setq close (if close-specified-p
                              close
                              default-close))
              (when close
                (finish-response socket *empty-chunk*))))))

      (etypecase body
        (null
         (write-response-headers socket status headers)
         (finish-response socket))
        (pathname (let ((stream (start-chunked-response socket status headers)))
                    (with-open-file (in body :direction :input :element-type '(unsigned-byte 8))
                      (copy-stream in stream))
                    (force-output stream)
                    (finish-response socket *empty-chunk*)))
        (list
         (as:write-socket-data
          socket
          (with-fast-output (buffer :vector)
            (cond
              ((getf headers :content-length)
               (response-headers-bytes buffer status headers (not close))
               (fast-write-crlf buffer)
               (loop for str in body
                     do (fast-write-sequence (string-to-utf-8-bytes str) buffer)))
              (T
               (cond
                 ((= (http-minor-version http) 1)
                  ;; Transfer-Encoding: chunked
                  (response-headers-bytes buffer status headers (not close))
                  (fast-write-sequence #.(string-to-utf-8-bytes "Transfer-Encoding: chunked") buffer)
                  (fast-write-crlf buffer)
                  (fast-write-crlf buffer)
                  (loop for str in body
                        do (fast-write-sequence
                            (string-to-utf-8-bytes (format nil "~X" (utf-8-byte-length str)))
                            buffer)
                           (fast-write-crlf buffer)
                           (fast-write-sequence (string-to-utf-8-bytes str) buffer)
                           (fast-write-crlf buffer))
                  (fast-write-byte #.(char-code #\0) buffer)
                  (fast-write-crlf buffer)
                  (fast-write-crlf buffer))
                 (T
                  ;; calculate Content-Length
                  (response-headers-bytes buffer status headers (not close))
                  (fast-write-sequence #.(string-to-utf-8-bytes "Content-Length: ") buffer)
                  (fast-write-sequence
                   (string-to-utf-8-bytes
                    (princ-to-string (loop for str in body
                                           sum (utf-8-byte-length str))))
                   buffer)
                  (fast-write-crlf buffer)
                  (fast-write-crlf buffer)
                  (loop for str in body
                        do (fast-write-sequence (string-to-utf-8-bytes str) buffer)))))))
          :write-cb (and close
                         (lambda (socket)
                           (setf (as:socket-data socket) nil)
                           (as:close-socket socket)))))
        ((vector (unsigned-byte 8))
         (write-response-headers socket status headers (not close))
         (when close
           (finish-response socket body)))))))
