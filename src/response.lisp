(in-package :cl-user)
(defpackage woo.response
  (:use :cl)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-byte-length)
  (:export :*empty-chunk*
           :*empty-bytes*
           :*crlf*
           :write-socket-string
           :write-socket-crlf
           :response-headers-bytes
           :write-response-headers
           :write-body-chunk
           :write-string-body-chunk
           :start-chunked-response
           :finish-response))
(in-package :woo.response)

(declaim (inline wev:write-socket-data wev:write-socket-byte))

(defun status-code-to-text (code)
  (cond
    ((< code 200)
     (case code
       (100 "Continue")
       (101 "Switching Protocols")
       (102 "Processing")))
    ((< code 300)
     (case code
       (200 "OK")
       (201 "Created")
       (202 "Accepted")
       (203 "Non-Authoritative Information")
       (204 "No Content")
       (205 "Reset Content")
       (206 "Partial Content")
       (207 "Multi-Status")
       (208 "Already Reported")
       (226 "IM Used")))
    ((< code 400)
     (case code
       (300 "Multiple Choices")
       (301 "Moved Permanently")
       (302 "Found")
       (303 "See Other")
       (304 "Not Modified")
       (305 "Use Proxy")
       (307 "Temporary Redirect")
       (308 "Permanent Redirect")))
    ((< code 500)
     (case code
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
       (417 "Expectation Failed")
       (418 "I'm a teapot")
       (421 "Misdirected Request")
       (422 "Unprocessable Entity")
       (423 "Locked")
       (424 "Failed Dependency")
       (426 "Upgrade Required")
       (451 "Unavailable For Legal Reasons")))
    (T
     (case code
       (500 "Internal Server Error")
       (501 "Not Implemented")
       (502 "Bad Gateway")
       (503 "Service Unavailable")
       (504 "Gateway Time-out")
       (505 "HTTP Version not supported")
       (506 "Variant Also Negotiates")
       (507 "Insufficient Storage")
       (508 "Loop Detected")
       (509 "Bandwidth Limit Exceeded")
       (510 "Not Extended")))))

(defvar *status-line* (make-hash-table :test 'eql))

(defun http/1.1 (code)
  (let ((status-text (status-code-to-text code)))
    (when status-text
      (format nil "HTTP/1.1 ~A ~A~C~C"
              code
              status-text
              #\Return
              #\Linefeed))))

(loop for status from 100 to 510
      for status-line = (http/1.1 status)
      when status-line
        do (setf (gethash status *status-line*)
                 (trivial-utf-8:string-to-utf-8-bytes status-line)))

(defvar *empty-chunk*
  #.(trivial-utf-8:string-to-utf-8-bytes (format nil "0~C~C~C~C"
                                                 #\Return #\Linefeed
                                                 #\Return #\Linefeed)))

(defvar *empty-bytes*
  #.(trivial-utf-8:string-to-utf-8-bytes ""))

(defvar *crlf*
  (trivial-utf-8:string-to-utf-8-bytes (format nil "~C~C" #\Return #\Linefeed)))

(declaim (inline write-socket-string write-socket-crlf))

(defun write-socket-string (socket string)
  (declare (optimize (speed 3) (safety 0)))
  (loop for char of-type character across string
        do (wev:write-socket-byte socket (char-code char))))

(defun write-socket-crlf (socket)
  (declare (optimize (speed 3) (safety 0)))
  (wev:write-socket-data socket *crlf*))

(declaim (type (simple-array character (29)) *date-header*))
(defvar *date-header* "Thu, 01 Jan 1970 00:00:00 GMT")

(declaim (inline integer-to-character))
(defun integer-to-character (int)
  (declare (type fixnum int)
           (optimize (speed 3) (safety 0)))
  (the character (code-char (+ 48 int))))

(defun current-rfc-1123-timestamp ()
  (declare (optimize (speed 3) (safety 0)))
  (macrolet ((write-date (val start &optional (len '*))
               `(replace *date-header*
                         (the (simple-array character (,len)) ,val)
                         :start1 ,start))
             (write-char-to-date (char idx)
               `(setf (aref *date-header* ,idx) ,char))
             (write-int-to-date (val start)
               (check-type start integer)
               `(if (< ,val 10)
                    (progn
                      (write-char-to-date #\0 ,start)
                      (write-char-to-date (integer-to-character ,val) ,(1+ start)))
                    (multiple-value-bind (quotient remainder)
                        (floor ,val 10)
                      (write-char-to-date (integer-to-character quotient) ,start)
                      (write-char-to-date (integer-to-character remainder) ,(1+ start))))))
    (multiple-value-bind (sec minute hour day month year weekday)
        (decode-universal-time (get-universal-time) 0)
      (declare (type fixnum sec minute hour day month year weekday))
      (write-date (svref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") weekday)
                  0 3)
      (write-int-to-date day 5)
      (write-date (svref #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                         month)
                  8 3)
      (multiple-value-bind (quotient remainder)
          (floor (the (unsigned-byte 64) year) 1000)
        (write-char-to-date (integer-to-character quotient) 12)
        (multiple-value-bind (quotient remainder)
            (floor remainder 100)
          (write-char-to-date (integer-to-character quotient) 13)
          (multiple-value-bind (quotient remainder)
              (floor remainder 10)
            (write-char-to-date (integer-to-character quotient) 14)
            (write-char-to-date (integer-to-character remainder) 15))))
      (write-int-to-date hour 17)
      (write-int-to-date minute 20)
      (write-int-to-date sec 23)))
  *date-header*)

(defun response-headers-bytes (socket status headers &optional keep-alive-p)
  (wev:write-socket-data socket (gethash status *status-line*))
  ;; Send default headers
  (wev:write-socket-data socket #.(string-to-utf-8-bytes "Date: "))
  (write-socket-string socket (the simple-string (current-rfc-1123-timestamp)))
  (write-socket-crlf socket)

  (when keep-alive-p
    (wev:write-socket-data
     socket
     #.(string-to-utf-8-bytes
        (format nil "Connection: keep-alive~C~C" #\Return #\Linefeed))))

  (loop for (k v) on headers by #'cddr
        when v
          do (write-socket-string socket (format nil "~:(~A~): ~A" k v))
             (write-socket-crlf socket)))

(defun write-response-headers (socket status headers &optional keep-alive-p)
  (response-headers-bytes socket status headers keep-alive-p)
  (write-socket-crlf socket))

(defun write-body-chunk (socket chunk &key (start 0) (end (length chunk)))
  (declare (optimize speed)
           (type fixnum start end)
           (type vector chunk))
  (unless (= start end)
    (wev:write-socket-data socket (map '(simple-array (unsigned-byte 8) (*))
                                       #'char-code
                                       (format nil "~X~C~C" (the fixnum (- end start)) #\Return #\Linefeed)))
    (wev:write-socket-data socket chunk :start start :end end)
    (wev:write-socket-data socket *crlf*)))

(defun write-string-body-chunk (socket chunk)
  (declare (optimize speed)
           (type string chunk))
  (unless (= 0 (length chunk))
    (wev:write-socket-data socket (map '(simple-array (unsigned-byte 8) (*))
                                       #'char-code
                                       (format nil "~X~C~C" (utf-8-byte-length chunk) #\Return #\Linefeed)))
    (write-socket-string socket chunk)
    (wev:write-socket-data socket *crlf*)))

(defun finish-response (socket &optional (body *empty-bytes*))
  (wev:write-socket-data socket body
                         :write-cb (lambda (socket)
                                     (wev:close-socket socket))))

(declaim (notinline wev:write-socket-data wev:write-socket-byte))
