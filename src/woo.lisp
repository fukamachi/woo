(in-package :cl-user)
(defpackage woo
  (:nicknames :clack.handler.woo)
  (:use :cl)
  (:import-from :woo.tcp
                :tcp-server-parallel)
  (:import-from :woo.response
                :*empty-chunk*
                :write-response-headers
                :start-chunked-response
                :finish-response)
  (:import-from :woo.url
                :url-decode
                :parse-url)
  (:import-from :fast-http
                :make-ll-parser
                :make-ll-callbacks
                :parser-method
                :parser-http-major
                :parser-http-minor
                :http-parse
                :parsing-error)
  (:import-from :fast-http.subseqs
                :byte-vector-subseqs-to-string
                :make-byte-vector-subseq)
  (:import-from :fast-http.byte-vector
                :ascii-octets-to-upper-string
                :byte-to-ascii-upper)
  (:import-from :fast-http.util
                :number-string-p
                :make-collector)
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
  (:import-from :lparallel
                :*kernel*
                :make-kernel)
  (:import-from :alexandria
                :hash-table-plist
                :copy-stream
                :if-let)
  (:export :run
           :stop))
(in-package :woo)

(cffi:define-foreign-library libevent2-pthreads
  (:darwin (:or
            "libevent_pthreads.dylib"
            "/usr/local/lib/libevent_pthreads.dylib"
            "/opt/local/lib/libevent_pthreads.dylib"))
  (:unix (:or "/usr/local/lib/event2/libevent_pthreads.so"
              "libevent_pthreads.so"
              "libevent_pthreads-2.0.so.5"
              "/usr/lib/libevent_pthreads.so"
              "/usr/local/lib/libevent_pthreads.so"))
  (t (:default "libevent_pthreads")))

(defvar *app* nil)
(defvar *debug* nil)

(defun run (app &key (debug t) (port 5000) (address "0.0.0.0")
                  (use-thread #+thread-support t
                              #-thread-support nil)
                  (kernel-count 1))
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
           (start-server-multi ()
             (as:with-event-loop (:catch-app-errors t)
               (tcp-server-parallel address port
                                    #'read-cb
                                    #'event-cb
                                    :connect-cb #'connect-cb)
               (bt:release-lock server-started-lock))))
      (when (< 1 kernel-count)
        #+unix
        (cffi:use-foreign-library libevent2-pthreads)
        (as:enable-threading-support)
        (setf lparallel:*kernel* (lparallel:make-kernel kernel-count
                                                        :bindings `((*app* . ,*app*)
                                                                    (*debug* . ,*debug*)))))
      (prog1
          (let ((start-fn (if (< 1 kernel-count)
                              #'start-server-multi
                              #'start-server)))
            (if use-thread
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

(defun canonicalize-header-field (data start end)
  (let ((byte (aref data start)))
    (if (or (= byte #.(char-code #\C))
            (= byte #.(char-code #\R))
            (= byte #.(char-code #\S))
            (= byte #.(char-code #\c))
            (= byte #.(char-code #\r))
            (= byte #.(char-code #\s)))
        (let ((field
                (intern (ascii-octets-to-upper-string data :start start :end end)
                        :keyword)))
          (if (find field '(:content-length
                            :content-type
                            :connection
                            :request-method
                            :script-name
                            :path-info
                            :server-name
                            :server-port
                            :server-protocol
                            :request-uri
                            :remote-addr
                            :remote-port
                            :query-string))
              field
              (intern (format nil "HTTP-~:@(~A~)" field) :keyword)))
        ;; This must be a custom header
        (let ((string (make-string (+ 5 (- end start)) :element-type 'character)))
          (loop for i from 0
                for char across "HTTP-"
                do (setf (aref string i) char))
          (do ((i 5 (1+ i))
               (j start (1+ j)))
              ((= j end) (intern string :keyword))
            (setf (aref string i)
                  (code-char (byte-to-ascii-upper (aref data j)))))))))

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

;; Using Low-level parser of fast-http
(defun setup-parser (socket)
  (let (headers env
        (body-buffer (fast-io::make-output-buffer))

        parsing-host-p
        parsing-connection-p

        resource
        url-path
        url-query
        method
        version
        host
        connection
        (headers-collector (make-collector))
        (header-value-collector nil)
        (current-len 0)

        completedp

        (parser (make-ll-parser :type :request))
        callbacks)
    (flet ((collect-prev-header-value ()
             (when header-value-collector
               (let* ((header-value
                        (byte-vector-subseqs-to-string
                         (funcall header-value-collector)
                         current-len))
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
                 (funcall headers-collector header-value)))))
      (setq callbacks
            (make-ll-callbacks
             :url (lambda (parser data start end)
                    (declare (ignore parser)
                             (type (simple-array (unsigned-byte 8) (*)) data)
                             (optimize (speed 3)))
                    (multiple-value-bind (path-start path-end query-start query-end)
                        (parse-url data start end)
                      (when path-start
                        (setq url-path (trivial-utf-8:utf-8-bytes-to-string (url-decode data path-start path-end))))
                      (when query-start
                        (setq url-query (trivial-utf-8:utf-8-bytes-to-string data :start query-start :end query-end))))
                    (setq resource (trivial-utf-8:utf-8-bytes-to-string data :start start :end end)))
             :header-field (lambda (parser data start end)
                             (declare (ignore parser)
                                      (type (simple-array (unsigned-byte 8) (*)) data)
                                      (optimize (speed 3)))
                             (collect-prev-header-value)
                             (setq header-value-collector (make-collector))
                             (setq current-len 0)

                             (let ((field (canonicalize-header-field data start end)))
                               (cond
                                 ((eq field :host)
                                  (setq parsing-host-p t))
                                 ((eq field :connection)
                                  (setq parsing-connection-p t)))
                               (funcall headers-collector field)))
             :header-value (lambda (parser data start end)
                             (declare (ignore parser)
                                      (type (simple-array (unsigned-byte 8) (*)) data)
                                      (optimize (speed 3)))
                             (incf current-len (- end start))
                             (funcall header-value-collector
                                      (make-byte-vector-subseq data start end)))
             :headers-complete (lambda (parser)
                                 (declare (type (simple-array (unsigned-byte 8) (*)) data)
                                          (optimize (speed 3)))
                                 (collect-prev-header-value)
                                 (setq version
                                       (http-version-keyword
                                        (parser-http-major parser)
                                        (parser-http-minor parser)))
                                 (setq method (parser-method parser))
                                 (setq headers (funcall headers-collector))
                                 (setq env (handle-request method
                                                           resource
                                                           url-path
                                                           url-query
                                                           version
                                                           host
                                                           headers
                                                           socket))
                                 (setq headers-collector nil
                                       header-value-collector nil))
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
                                   (funcall *app* env)
                                   env
                                   connection))
                T))))))

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
    (nconc
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
           :clack.io socket)

     ;; FIXME: Concat duplicate headers with a comma.
     headers)))


;;
;; Handling responses

(defun handle-response (socket clack-res request-headers connection)
  (handler-case
      (etypecase clack-res
        (list (handle-normal-response socket clack-res request-headers connection))
        (function (funcall clack-res (lambda (clack-res)
                                       (handler-case
                                           (handle-normal-response socket clack-res request-headers connection)
                                         (as:socket-closed ()))))))
    (as:tcp-error (e)
      (log:error e))
    (t (e)
      (log:error e))))

(defun handle-normal-response (socket clack-res request-headers connection)
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
         (setf body
               (fast-io:with-fast-output (buffer :vector)
                 (loop with content-length = 0
                       for str in body
                       do (let ((bytes (trivial-utf-8:string-to-utf-8-bytes str :encoding :utf-8)))
                            (fast-io:fast-write-sequence bytes buffer)
                            (incf content-length (length bytes)))
                       finally
                          (unless (getf headers :content-length)
                            (setf headers
                                  (append headers
                                          (list :content-length content-length)))))))

         (write-response-headers socket status headers)

         (as:write-socket-data socket body)

         (if (string= connection "close")
             (finish-response socket)
             (setup-parser socket)))
        ((vector (unsigned-byte 8))
         (write-response-headers socket status headers)
         (if (string= connection "close")
             (finish-response socket body)
             (setup-parser socket)))))))
