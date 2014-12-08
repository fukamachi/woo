(in-package :cl-user)
(defpackage woo.response
  (:use :cl)
  (:import-from :cl-async
                :delay
                :socket-data
                :write-socket-data
                :async-io-stream
                :close-socket
                :socket-c
                :*buffer-size*
                :*socket-buffer-c*)
  (:import-from :cl-async-util
                :save-callbacks)
  (:import-from :chunga
                :make-chunked-stream
                :chunked-stream-output-chunking-p )
  (:import-from :libevent2
                :bufferevent-get-output
                :bufferevent-enable
                :evbuffer-add
                :+ev-read+
                :+ev-write+)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:import-from :alexandria
                :with-gensyms)
  (:export :*empty-chunk*
           :*empty-bytes*
           :with-evbuffer
           :write-octets-to-evbuffer
           :write-ascii-string-to-evbuffer
           :write-crlf-to-evbuffer
           :response-headers-bytes
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

;;
;; Utilities for writing to libevent2 buffer directly

(declaim (type fixnum *buffer-index*))
(defvar *buffer-index* 0)

(defmacro defun-evbuffer-writer (name data-type &optional key)
  `(defun ,name (data evbuffer)
     (declare (type ,data-type data)
              (optimize (speed 3) (safety 2)))
     (let ((data-length (length data))
           (data-index 0))
       (declare (type fixnum data-length data-index)
                (dynamic-extent data-index))
       (loop until (zerop data-length) do
         (let* ((buffer-left (- (the fixnum as::*buffer-size*) (the fixnum *buffer-index*)))
                (buffer-run-out-p (< buffer-left data-length))
                (bufsize (if buffer-run-out-p buffer-left data-length)))
           (declare (type boolean buffer-run-out-p)
                    (type fixnum bufsize buffer-left))
           (dotimes (i bufsize)
             (declare (ignore i))
             (setf (cffi:mem-aref as::*socket-buffer-c* :unsigned-char (the fixnum *buffer-index*))
                   ,(if key
                        `(,key (aref data data-index))
                        `(aref data data-index)))
             (incf data-index)
             (incf *buffer-index*))
           (if buffer-run-out-p
               (progn
                 (le:evbuffer-add evbuffer as::*socket-buffer-c* as::*buffer-size*)
                 (setq *buffer-index* 0)
                 (decf data-length bufsize))
               (return)))))))

(defun-evbuffer-writer write-octets-to-evbuffer (simple-array (unsigned-byte 8) (*)))
(defun-evbuffer-writer write-ascii-string-to-evbuffer simple-string char-code)
(defun write-crlf-to-evbuffer (evbuffer)
  (write-octets-to-evbuffer #.(string-to-utf-8-bytes (format nil "~C~C" #\Return #\Newline)) evbuffer))

(defmacro with-evbuffer ((evbuffer socket &key read-cb write-cb event-cb) &body body)
  (with-gensyms (bev bev-data socket-data-pointer callbacks do-send)
    `(let* ((,bev (as::socket-c ,socket))
            (,evbuffer (le:bufferevent-get-output ,bev))
            (*buffer-index* 0))
       (flet ((,do-send ()
                (le:bufferevent-enable ,bev (logior le:+ev-read+ le:+ev-write+))
                ,@body
                (locally (declare (optimize (speed 3) (safety 0)))
                  (unless (= (the fixnum *buffer-index*) 0)
                    (le:evbuffer-add ,evbuffer as::*socket-buffer-c* *buffer-index*)
                    (setq *buffer-index* 0)))))
         ,(if (or read-cb write-cb event-cb)
              `(as:delay
                (lambda ()
                  (let* ((,bev-data (as::deref-data-from-pointer ,bev))
                         (,socket-data-pointer (getf ,bev-data :data-pointer))
                         (,callbacks (as::get-callbacks ,socket-data-pointer)))
                    (save-callbacks ,socket-data-pointer
                                    (list :read-cb ,(or read-cb `(getf ,callbacks :read-cb))
                                          :write-cb ,(or write-cb `(getf ,callbacks :write-cb))
                                          :event-cb ,(or event-cb `(getf ,callbacks :event-cb)))))
                  (,do-send)))
              `(,do-send))))))

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

(defun response-headers-bytes (evbuffer status headers &optional keep-alive-p)
  (write-octets-to-evbuffer (gethash status *status-line*) evbuffer)
  ;; Send default headers
  (write-octets-to-evbuffer #.(string-to-utf-8-bytes "Date: ") evbuffer)
  (write-ascii-string-to-evbuffer (current-rfc-1123-timestamp) evbuffer)
  (write-crlf-to-evbuffer evbuffer)

  (when keep-alive-p
    (write-octets-to-evbuffer
     #.(string-to-utf-8-bytes
        (format nil "Connection: keep-alive~C~C" #\Return #\Newline))
     evbuffer))

  (loop for (k v) on headers by #'cddr
        when v
          do (write-ascii-string-to-evbuffer (format nil "~:(~A~): ~A" k v) evbuffer)
             (write-crlf-to-evbuffer evbuffer)))

(defun write-response-headers (socket status headers &optional keep-alive-p)
  (with-evbuffer (evbuffer socket)
    (response-headers-bytes evbuffer status headers keep-alive-p)
    (write-crlf-to-evbuffer evbuffer)))

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
