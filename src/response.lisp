(in-package :cl-user)
(defpackage woo.response
  (:use :cl)
  (:import-from :cl-async
                :socket-data
                :write-socket-data
                :async-io-stream
                :close-socket)
  (:import-from :chunga
                :make-chunked-stream
                :chunked-stream-output-chunking-p )
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:export :*empty-chunk*
           :*empty-bytes*
           :write-response-headers
           :start-chunked-response
           :finish-response))
(in-package :woo.response)

(defun status-code-to-text (code)
  (cond
    ((< code 200)
     (ecase code
       (100 "Continue")
       (101 "Switching Protocols")))
    ((< code 300)
     (ecase code
       (200 "OK")
       (201 "Created")
       (202 "Accepted")
       (203 "Non-Authoritative Information")
       (204 "No Content")
       (205 "Reset Content")
       (206 "Partial Content")))
    ((< code 400)
     (ecase code
       (300 "Multiple Choices")
       (301 "Moved Permanently")
       (302 "Found")
       (303 "See Other")
       (304 "Not Modified")
       (305 "Use Proxy")
       (307 "Temporary Redirect")))
    ((< code 500)
     (ecase code
       (400 "Bad Request")
       (401 "Unauthorized")
       (402 "Payment Required")
       (403 "Forbidden")
       (404 "Not Found")
       (405 "Method Not Allowed")
       (406 "Not Acceptable")
       (407 "Proxy Authentication Required")
       (408 "Request Time-out")
       (409 "Conflict")
       (410 "Gone")
       (411 "Length Required")
       (412 "Precondition Failed")
       (413 "Request Entity Too Large")
       (414 "Request-URI Too Large")
       (415 "Unsupported Media Type")
       (416 "Requested range not satisfiable")
       (417 "Expectation Failed")))
    ((<= 500 code)
     (ecase code
       (500 "Internal Server Error")
       (501 "Not Implemented")
       (502 "Bad Gateway")
       (503 "Service Unavailable")
       (504 "Gateway Time-out")
       (505 "HTTP Version not supported")))
    (T (error "Invalid status code: ~A" code))))

(defvar *status-line* (make-hash-table :test 'eql))

(defun http/1.1 (code)
  (format nil "HTTP/1.1 ~A ~A~C~C"
          code
          (status-code-to-text code)
          #\Return
          #\Newline))

(loop for status in '(100 101
                      200 201 202 203 204 205 206
                      300 301 302 303 304 305 307
                      400 401 402 403 404 405 406 407 408 409 410 411 412 413 414 415 416 417
                      500 501 502 503 504 505)
      do (setf (gethash status *status-line*)
               (trivial-utf-8:string-to-utf-8-bytes (http/1.1 status))))

(defvar *empty-chunk*
  #.(trivial-utf-8:string-to-utf-8-bytes (format nil "0~C~C~C~C"
                                                 #\Return #\Newline
                                                 #\Return #\Newline)))

(defvar *empty-bytes*
  #.(trivial-utf-8:string-to-utf-8-bytes ""))

(defvar +date-header-format+
  (append
   (list "Date: ")
   local-time:+rfc-1123-format+
   (list (format nil "~C~C" #\Return #\Newline))))

(defun write-response-headers (socket status headers &optional keep-alive-p)
  (as:write-socket-data
   socket
   (fast-io:with-fast-output (buffer :vector)
     (fast-io:fast-write-sequence (gethash status *status-line*) buffer)
     ;; Send default headers
     (fast-io:fast-write-sequence (trivial-utf-8:string-to-utf-8-bytes
                                   (local-time:format-timestring nil
                                                                 (local-time:now)
                                                                 :format +date-header-format+))
                                  buffer)
     (when keep-alive-p
       (fast-io:fast-write-sequence
        #.(trivial-utf-8:string-to-utf-8-bytes
           (format nil "Connection: keep-alive~C~C" #\Return #\Newline))
        buffer))

     (loop for (k v) on headers by #'cddr
           when v
             do (fast-io:fast-write-sequence
                 (trivial-utf-8:string-to-utf-8-bytes (format nil "~:(~A~): ~A~C~C"
                                                              k v #\Return #\Newline))
                 buffer))
     (fast-io:fast-write-byte (char-code #\Return) buffer)
     (fast-io:fast-write-byte (char-code #\Newline) buffer))))

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
