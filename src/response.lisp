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
  (:import-from :babel
                :string-to-octets)
  (:export :*empty-chunk*
           :*empty-bytes*
           :write-response-headers
           :start-chunked-response
           :finish-response))
(in-package :woo.response)

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
