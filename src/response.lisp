(in-package :cl-user)
(defpackage woo.response
  (:use :cl)
  (:import-from :fast-http
                :make-http-response
                :http-unparse)
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

(defvar *status-line* (make-hash-table :test 'eql))

(defun http/1.1 (code)
  (format nil "HTTP/1.1 ~A ~A~C~C"
          code
          (fast-http.unparser::status-code-to-text code)
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
  (as:write-socket-data socket
                        (gethash status *status-line*))
  ;; Send default headers
  (as:write-socket-data socket
                        (local-time:format-timestring nil
                                                      (local-time:now)
                                                      :format +date-header-format+))
  (when keep-alive-p
    (as:write-socket-data socket
                          #.(trivial-utf-8:string-to-utf-8-bytes
                             (format nil "Connection: keep-alive~C~C" #\Return #\Newline))))
  (loop for (k v) on headers by #'cddr
        when v
          do (as:write-socket-data socket
                                   (format nil "~:(~A~): ~A~C~C"
                                           k v #\Return #\Newline)))
  (as:write-socket-data socket #.(trivial-utf-8:string-to-utf-8-bytes (format nil "~C~C" #\Return #\Newline))))

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