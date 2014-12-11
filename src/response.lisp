(in-package :cl-user)
(defpackage woo.response
  (:use :cl)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-sequence
                :fast-write-byte)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:export :*empty-chunk*
           :*empty-bytes*
           :fast-write-crlf
           :response-headers-bytes
           :write-response-headers
           :write-body-chunk
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

(declaim (inline fast-write-string fast-write-crlf))

(defun fast-write-string (string buffer)
  (declare (optimize (speed 3) (safety 0)))
  (loop for char of-type character across string
        do (fast-write-byte (char-code char) buffer)))

(defun fast-write-crlf (buffer)
  (declare (optimize (speed 3) (safety 0)))
  (fast-write-byte #.(char-code #\Return) buffer)
  (fast-write-byte #.(char-code #\Newline) buffer))

(declaim (type (simple-array character (31)) *date-header*))
(defvar *date-header* (make-string 31))

(declaim (inline integer-to-character))
(defun integer-to-character (int)
  (declare (type fixnum int)
           (optimize (speed 3) (safety 0)))
  (the character (code-char (+ 48 int))))

(defun current-time ()
  (declare (optimize (speed 3) (safety 0)))
  (flet ((timezone-offset (unix-time timezone)
           (declare (type fixnum unix-time))
           (let* ((zone (local-time::%realize-timezone timezone))
                  (subzone-idx (if (zerop (length (local-time::timezone-indexes zone)))
                                   0
                                   (elt (local-time::timezone-indexes zone)
                                        (local-time::transition-position unix-time
                                                                         (local-time::timezone-transitions zone)))))
                  (subzone (svref (local-time::timezone-subzones zone) subzone-idx)))
             (declare (type fixnum subzone-idx))
             (local-time::subzone-offset subzone))))
    (let* ((timezone local-time:*default-timezone*)
           (sec (local-time::%get-current-time))
           (offset (timezone-offset sec timezone)))
      (declare (type fixnum sec))
      (multiple-value-bind (days secs)
          (floor (the (unsigned-byte 64) sec) local-time:+seconds-per-day+)
        (decf days 11017)
        (multiple-value-bind (adjusted-secs adjusted-days)
            (local-time::%adjust-to-offset secs days offset)
          (declare (type fixnum adjusted-secs adjusted-days))
          (multiple-value-bind (hours minutes seconds)
              (local-time::%timestamp-decode-time adjusted-secs)
            (multiple-value-bind (year month day)
                (local-time::%timestamp-decode-date adjusted-days)
              (values
               seconds
               minutes
               hours
               day
               month
               year
               (mod (the (unsigned-byte 64) (+ 3 adjusted-days)) 7)
               offset))))))))

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
    (multiple-value-bind (sec minute hour day month year weekday offset)
        (current-time)
      (write-date (svref local-time::+short-day-names+ weekday) 0 3)
      (write-date ", " 3 2)
      (write-int-to-date day 5)
      (write-char-to-date #\Space 7)
      (write-date (svref local-time::+short-month-names+ month) 8 3)
      (write-char-to-date #\Space 11)
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
      (write-char-to-date #\Space 16)
      (write-int-to-date hour 17)
      (write-char-to-date #\: 19)
      (write-int-to-date minute 20)
      (write-char-to-date #\: 22)
      (write-int-to-date sec 23)
      (write-char-to-date #\Space 25)
      (multiple-value-bind (offset-hours offset-secs)
          (floor (the (unsigned-byte 64) offset) local-time::+seconds-per-hour+)
        (declare (fixnum offset-hours offset-secs))
        (write-char-to-date (if (minusp offset-hours) #\- #\+) 26)
        (write-int-to-date (abs offset-hours) 27)
        (let ((offset-min (truncate (the (unsigned-byte 64) (abs offset-secs))
                                    local-time::+seconds-per-minute+)))
          (write-int-to-date offset-min 29)))))
  *date-header*)

(defun response-headers-bytes (buffer status headers &optional keep-alive-p)
  (fast-write-sequence (gethash status *status-line*) buffer)
  ;; Send default headers
  (fast-write-sequence #.(string-to-utf-8-bytes "Date: ") buffer)
  (fast-write-string (current-rfc-1123-timestamp) buffer)
  (fast-write-crlf buffer)

  (when keep-alive-p
    (fast-write-sequence
     #.(string-to-utf-8-bytes
        (format nil "Connection: keep-alive~C~C" #\Return #\Newline))
     buffer))

  (loop for (k v) on headers by #'cddr
        when v
          do (fast-write-string (format nil "~:(~A~): ~A" k v) buffer)
             (fast-write-crlf buffer)))

(defun write-response-headers (socket status headers &optional keep-alive-p)
  (wev:write-socket-data
   socket
   (with-fast-output (buffer :vector)
     (response-headers-bytes buffer status headers keep-alive-p)
     (fast-write-crlf buffer))))

(defun write-body-chunk (socket chunk &key (start 0) (end (length chunk)))
  (check-type chunk (simple-array (unsigned-byte 8) (*)))
  (wev:write-socket-data socket (map '(simple-array (unsigned-byte 8) (*))
                                     #'char-code
                                     (format nil "~X~C~C" (- end start) #\Return #\Newline)))
  (wev:write-socket-data socket chunk :start start :end end)
  (wev:write-socket-data socket #.(string-to-utf-8-bytes (format nil "~C~C" #\Return #\Newline))))

(defun finish-response (socket &optional (body *empty-bytes*))
  (wev:write-socket-data socket body
                         :write-cb (lambda (socket)
                                     (wev:close-socket socket))))
