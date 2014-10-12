(in-package :cl-user)
(defpackage woo
  (:nicknames :clack.handler.woo)
  (:use :cl)
  (:import-from :fast-http
                :make-parser
                :make-http-request
                :make-http-response
                :http-unparse
                :http-resource
                :http-method
                :http-version
                :http-body
                :parsing-error)
  (:import-from :puri
                :parse-uri-string)
  (:import-from :do-urlencode
                :urldecode)
  (:import-from :cl-async
                :async-io-stream
                :socket-closed
                :write-socket-data
                :socket-data
                :close-socket)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-byte
                :fast-write-sequence)
  (:import-from :chunga
                :make-chunked-stream
                :chunked-stream-output-chunking-p)
  (:import-from :babel
                :string-to-octets)
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

(defun run (app &key (debug t) (port 5000)
                  (use-thread #+thread-support t
                              #-thread-support nil))
  (let ((server-started-lock (bt:make-lock "server-started")))
    (flet ((start-server ()
             (as:with-event-loop (:catch-app-errors t)
               (as:tcp-server "0.0.0.0" port
                              #'read-cb
                              (lambda (event)
                                (typecase event
                                  (as:tcp-eof ())
                                  (as:tcp-error ()
                                   (log:error (princ-to-string event)))
                                  (T
                                   (log:info (princ-to-string event))
                                   (let ((socket (as:tcp-socket event)))
                                     (write-response-headers socket 500 ())
                                     (as:write-socket-data socket "Internal Server Error"
                                                           :write-cb (lambda (socket)
                                                                       (setf (as:socket-data socket) nil)
                                                                       (as:close-socket socket)))))))
                              :connect-cb (lambda (socket) (setup-parser socket app debug)))
               (bt:release-lock server-started-lock))))
      (prog1
          (if use-thread
              (bt:make-thread #'start-server)
              (start-server))
        (bt:acquire-lock server-started-lock t)
        (sleep 0.05)))))

(defun read-cb (socket data)
  (let ((parser (getf (as:socket-data socket) :parser)))
    (handler-case (funcall parser data)
      (fast-http:parsing-error (e)
        (log:error "fast-http parsing error: ~A" e)
        (write-response-headers socket 400 ())
        (finish-response socket (princ-to-string e)))
      (fast-http:fast-http-error (e)
        (log:error "fast-http error: ~A" e)
        (write-response-headers socket 500 ())
        (finish-response socket #.(babel:string-to-octets "Internal Server Error"))))))

(defun setup-parser (socket app debug)
  (let ((http (make-http-request))
        headers env
        (body-buffer (fast-io::make-output-buffer)))
    (setf (getf (as:socket-data socket) :parser)
          (make-parser http
                       :header-callback
                       (lambda (headers-plist)
                         (setq headers headers-plist
                               env (handle-request http headers-plist socket)))
                       :body-callback
                       (lambda (data)
                         (fast-io:fast-write-sequence data body-buffer))
                       :finish-callback
                       (lambda ()
                         (setf env
                               (nconc (list :raw-body
                                            (flex:make-in-memory-input-stream (fast-io::finish-output-buffer body-buffer)))
                                      env))
                         (handle-response
                          socket
                          (if debug
                              (funcall app env)
                              (if-let (res (handler-case (funcall app env)
                                             (error (error)
                                               (princ error *error-output*)
                                               nil)))
                                res
                                '(500 nil nil)))
                          headers
                          app
                          debug))))))

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

(defun handle-request (http headers socket)
  (let ((host (getf headers :host)))
    (multiple-value-bind (server-name server-port)
        (parse-host-header host)
      (multiple-value-bind (scheme host port path query)
          (puri::parse-uri-string (http-resource http))
        (declare (ignore scheme host port))
        (nconc
         (list :request-method (http-method http)
               :script-name ""
               :server-name server-name
               :server-port (or server-port 80)
               :server-protocol (intern (format nil "HTTP/~F" (http-version http)) :keyword)
               :path-info (do-urlencode:urldecode path :lenientp t)
               :query-string query
               :url-scheme :http
               :request-uri (http-resource http)
               :content-length (getf headers :content-length)
               :content-type (getf headers :content-type)
               :clack.streaming t
               :clack.nonblocking t
               :clack.io socket)

         (loop with env-hash = (make-hash-table :test 'eq)
               for (key val) on headers by #'cddr
               unless (find key '(:request-method
                                  :script-name
                                  :path-info
                                  :server-name
                                  :server-port
                                  :server-protocol
                                  :request-uri
                                  :remote-addr
                                  :remote-port
                                  :query-string
                                  :content-length
                                  :content-type
                                  :connection))
                 do
                    (let ((key (intern (format nil "HTTP-~:@(~A~)" key) :keyword)))
                      (if (gethash key env-hash)
                          (setf (gethash key env-hash)
                                (concatenate 'string (gethash key env-hash) ", " val))
                          (setf (gethash key env-hash) val)))
               finally
                  (return (hash-table-plist env-hash))))))))


;;
;; Handling responses

(defvar *empty-chunk*
  #.(babel:string-to-octets (format nil "0~C~C~C~C"
                                    #\Return #\Newline
                                    #\Return #\Newline)))

(defvar *empty-bytes*
  #.(babel:string-to-octets ""))

(defun write-response-headers (socket status headers)
  (fast-http:http-unparse (make-http-response :status status
                                              :headers headers)
                          (lambda (data)
                            (as:write-socket-data socket data))))

(defun start-chunked-response (socket status headers)
  (write-response-headers socket status (append headers
                                                (list :transfer-encoding "chunked")))

  (let* ((async-stream (make-instance 'as:async-io-stream :socket socket))
         (chunked-stream (chunga:make-chunked-stream async-stream)))
    (setf (chunga:chunked-stream-output-chunking-p chunked-stream) t)
    chunked-stream))

(defun finish-response (socket &optional (body *empty-bytes*))
  (as:write-socket-data socket body
                        :write-cb (lambda (socket)
                                    (setf (as:socket-data socket) nil)
                                    (as:close-socket socket))))

(defun handle-response (socket clack-res request-headers app debug)
  (etypecase clack-res
    (list (handle-normal-response socket clack-res request-headers app debug))
    (function (funcall clack-res (lambda (clack-res)
                                   (handler-case
                                       (handle-normal-response socket clack-res request-headers app debug)
                                     (as:socket-closed ())))))))

(defun handle-normal-response (socket clack-res request-headers app debug)
  (let ((no-body '#:no-body))
    (destructuring-bind (status headers &optional (body no-body))
        clack-res
      (when (eq body no-body)
        (let* ((stream (start-chunked-response socket status headers))
               (connection (getf request-headers :connection))
               (default-close (cond
                                ((string= connection "keep-alive") nil)
                                ((string= connection "close") t))))
          (return-from handle-normal-response
            (lambda (body &key (close nil close-specified-p))
              (etypecase body
                (string (write-sequence (babel:string-to-octets body) stream))
                (vector (write-sequence body stream)))
              (force-output stream)
              (setq close (if close-specified-p
                              close
                              default-close))
              (if close
                  (finish-response socket *empty-chunk*)
                  (setup-parser socket app debug))))))

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
         (setf body
               (fast-io:with-fast-output (buffer :vector)
                 (loop with content-length = 0
                       for str in body
                       do (let ((bytes (babel:string-to-octets str :encoding :utf-8)))
                            (fast-io:fast-write-sequence bytes buffer)
                            (incf content-length (length bytes)))
                       finally
                          (unless (getf headers :content-length)
                            (setf headers
                                  (append headers
                                          (list :content-length content-length)))))))

         (write-response-headers socket status headers)

         (as:write-socket-data socket body)

         (if (string= (getf request-headers :connection) "close")
             (finish-response socket)
             (setup-parser socket app debug)))
        ((vector (unsigned-byte 8))
         (write-response-headers socket status headers)
         (if (string= (getf request-headers :connection) "close")
             (finish-response socket body)
             (setup-parser socket app debug)))))))
