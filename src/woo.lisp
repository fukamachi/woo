(in-package :cl-user)
(defpackage woo
  (:nicknames :clack.handler.woo)
  (:use :cl)
  (:import-from :woo.response
                :*empty-chunk*
                :write-response-headers
                :start-chunked-response
                :finish-response)
  (:import-from :woo.url
                :parse-url)
  (:import-from :quri
                :url-decode
                :url-decoding-error)
  (:import-from :fast-http
                :make-ll-parser
                :make-ll-callbacks
                :parser-method
                :parser-http-major
                :parser-http-minor
                :http-parse
                :parsing-error)
  (:import-from :fast-http.byte-vector
                :ascii-octets-to-lower-string)
  (:import-from :fast-http.util
                :number-string-p)
  (:import-from :xsubseq
                :coerce-to-string
                :make-null-concatenated-xsubseqs
                :xnconcf
                :xsubseq)
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
                :fast-write-byte
                :fast-write-sequence)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string)
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

(defun http-version-keyword (major minor)
  (cond
    ((= major 1)
     (cond
       ((= minor 1) :HTTP/1.1)
       ((= minor 0) :HTTP/1.0)
       (T (intern (format nil "HTTP/1.~A" minor) :keyword))))
    ((= major 2)
     (cond
       ((= minor 0) :HTTP/2.0)
       (T (intern (format nil "HTTP/2.~A" minor) :keyword))))
    (T (intern (format nil "HTTP/~A.~A" major minor) :keyword))))

(declaim (inline fast-http.util:number-string-p))
;; Using Low-level parser of fast-http
(defun setup-parser (socket)
  (let ((headers (make-hash-table :test 'equal))
        env
        (body-buffer (fast-io::make-output-buffer))

        parsing-header-field
        parsing-host-p
        parsing-connection-p

        resource
        url-path
        url-query
        method
        version
        host
        connection
        (header-value-collector nil)

        completedp

        (parser (make-ll-parser :type :request))
        callbacks)
    (flet ((collect-prev-header-value ()
             (when header-value-collector
               (let* ((header-value
                        (coerce-to-string header-value-collector))
                      (header-value
                        (if (number-string-p header-value)
                            (read-from-string header-value)
                            header-value)))
                 (cond
                   (parsing-host-p
                    (setq host header-value
                          parsing-host-p nil))
                   (parsing-connection-p
                    (setq connection header-value
                          parsing-connection-p nil)))
                 (multiple-value-bind (previous-value existsp)
                     (gethash parsing-header-field headers)
                   (setf (gethash parsing-header-field headers)
                         (if existsp
                             (format nil "~A, ~A" previous-value header-value)
                             header-value)))))))
      (setq callbacks
            (make-ll-callbacks
             :url (lambda (parser data start end)
                    (declare (ignore parser)
                             (type (simple-array (unsigned-byte 8) (*)) data)
                             (optimize (speed 3)))
                    (multiple-value-bind (path-start path-end query-start query-end)
                        (parse-url data start end)
                      (when path-start
                        (setq url-path
                              (handler-case
                                  (url-decode data :start path-start :end path-end)
                                (url-decoding-error ()
                                  (trivial-utf-8:utf-8-bytes-to-string data
                                                                       :start path-start
                                                                       :end path-end)))))
                      (when query-start
                        (setq url-query (trivial-utf-8:utf-8-bytes-to-string data :start query-start :end query-end))))
                    (setq resource (trivial-utf-8:utf-8-bytes-to-string data :start start :end end)))
             :header-field (lambda (parser data start end)
                             (declare (ignore parser)
                                      (type (simple-array (unsigned-byte 8) (*)) data)
                                      (optimize (speed 3)))
                             (collect-prev-header-value)
                             (setq header-value-collector (make-null-concatenated-xsubseqs))

                             (let ((field (ascii-octets-to-lower-string data :start start :end end)))
                               (cond
                                 ((string= field "host")
                                  (setq parsing-host-p t))
                                 ((string= field "connection")
                                  (setq parsing-connection-p t)))
                               (setq parsing-header-field field)))
             :header-value (lambda (parser data start end)
                             (declare (ignore parser)
                                      (type (simple-array (unsigned-byte 8) (*)) data)
                                      (optimize (speed 3)))
                             (xnconcf header-value-collector
                                      (xsubseq data start end)))
             :headers-complete (lambda (parser)
                                 (declare (optimize (speed 3)))
                                 (collect-prev-header-value)
                                 (setq version
                                       (http-version-keyword
                                        (parser-http-major parser)
                                        (parser-http-minor parser)))
                                 (setq method (parser-method parser))
                                 (setq env (handle-request method
                                                           resource
                                                           url-path
                                                           url-query
                                                           version
                                                           host
                                                           headers
                                                           socket))
                                 (setq header-value-collector nil))
             :body (lambda (parser data start end)
                     (declare (ignore parser)
                              (type (simple-array (unsigned-byte 8) (*)) data)
                              (optimize (speed 3)))
                     (do ((i start (1+ i)))
                         ((= i end))
                       (fast-write-byte (aref data i) body-buffer)))
             :message-complete (lambda (parser)
                                 (declare (ignore parser))
                                 (collect-prev-header-value)
                                 (setq completedp t))))
      (setf (as:socket-data socket)
            (alexandria:named-lambda parse-execute (data)
              (when completedp
                (return-from parse-execute T))
              (http-parse parser callbacks data)
              (when completedp
                (let ((env (nconc (list :raw-body
                                        (flex:make-in-memory-input-stream
                                         (fast-io::finish-output-buffer body-buffer)))
                                  env)))
                  (handle-response socket
                                   (if *debug*
                                       (funcall *app* env)
                                       (if-let (res (handler-case (funcall *app* env)
                                                      (error (error)
                                                        (log:error error)
                                                        nil)))
                                         res
                                         '(500 nil nil)))
                                   connection))
                T))))))
(declaim (notinline fast-http.util:number-string-p))

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

(defun handle-request (method resource path query version host headers socket)
  (multiple-value-bind (server-name server-port)
      (if host
          (parse-host-header host)
          (values nil nil))
    (list :request-method method
          :script-name ""
          :server-name server-name
          :server-port (or server-port 80)
          :server-protocol version
          :path-info path
          :query-string query
          :url-scheme :http
          :request-uri resource
          :clack.streaming t
          :clack.nonblocking t
          :clack.io socket
          :headers headers)))


;;
;; Handling responses

(defun handle-response (socket clack-res connection)
  (handler-case
      (etypecase clack-res
        (list (handle-normal-response socket clack-res connection))
        (function (funcall clack-res (lambda (clack-res)
                                       (handler-case
                                           (handle-normal-response socket clack-res connection)
                                         (as:socket-closed ()))))))
    (as:tcp-error (e)
      (log:error e))
    (t (e)
      (log:error e))))

(defun handle-normal-response (socket clack-res connection)
  (let ((no-body '#:no-body))
    (destructuring-bind (status headers &optional (body no-body))
        clack-res
      (when (eq body no-body)
        (let* ((stream (start-chunked-response socket status headers))
               (default-close (cond
                                ((string= connection "keep-alive") nil)
                                ((string= connection "close") t))))
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
         (let ((close (string= connection "close")))
           (setf body
                 (fast-io:with-fast-output (buffer :vector)
                   (loop with content-length = 0
                         for str in body
                         do (let ((bytes (trivial-utf-8:string-to-utf-8-bytes str)))
                              (fast-io:fast-write-sequence bytes buffer)
                              (incf content-length (length bytes)))
                         finally
                            (unless (getf headers :content-length)
                              (setf headers
                                    (append headers
                                            (list :content-length content-length)))))))

           (write-response-headers socket status headers (not close))

           (as:write-socket-data socket body)

           (if close
               (finish-response socket)
               (setup-parser socket))))
        ((vector (unsigned-byte 8))
         (let ((close (string= connection "close")))
           (write-response-headers socket status headers (not close))
           (if close
               (finish-response socket body)
               (setup-parser socket))))))))
