(in-package :cl-user)
(defpackage woo-test.body-limit
  (:use :cl
        :rove)
  (:import-from :clack.test
                :testing-app
                :*clack-test-access-port*)
  (:import-from :clack.test.suite
                :localhost)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string))
(in-package :woo-test.body-limit)

(defparameter *memory-limit* 16384)
(defparameter *disk-limit* 65536)

(defvar *body-file* nil)

(defmacro with-body-limits (&body body)
  `(let ((memory-limit smart-buffer:*default-memory-limit*)
         (disk-limit smart-buffer:*default-disk-limit*))
     (setf smart-buffer:*default-memory-limit* *memory-limit*
           smart-buffer:*default-disk-limit* *disk-limit*
           *body-file* nil)
     (unwind-protect (progn ,@body)
       (setf smart-buffer:*default-memory-limit* memory-limit
             smart-buffer:*default-disk-limit* disk-limit))))

(defun body-files ()
  (assert *body-file*)
  (uiop:directory-files (uiop:pathname-directory-pathname *body-file*)))

(defun new-body-files (before)
  (set-difference (body-files) before :test #'equal))

(defmacro with-connection ((stream) &body body)
  (let ((socket (gensym "SOCKET")))
    `(let* ((,socket (usocket:socket-connect "127.0.0.1" *clack-test-access-port*
                                             :element-type '(unsigned-byte 8)))
            (,stream (usocket:socket-stream ,socket)))
       (unwind-protect (progn ,@body)
         (ignore-errors (usocket:socket-close ,socket))))))

(defun send (stream &rest parts)
  (handler-case
      (progn
        (dolist (part parts)
          (write-sequence (if (stringp part) (string-to-utf-8-bytes part) part) stream))
        (finish-output stream)
        nil)
    (error (e) e)))

(defun send-request (stream header &key (body-size 0) chunked)
  (let ((chunk (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 97)))
    (or (send stream (format nil "POST / HTTP/1.1~C~CHost: localhost~C~C~A~C~C~C~C"
                             #\Return #\Newline #\Return #\Newline header
                             #\Return #\Newline #\Return #\Newline))
        (loop for sent from 0 below body-size by (length chunk)
              for size = (min (length chunk) (- body-size sent))
              for error = (if chunked
                              (send stream (format nil "~X~C~C" size #\Return #\Newline)
                                    (subseq chunk 0 size)
                                    (format nil "~C~C" #\Return #\Newline))
                              (send stream (subseq chunk 0 size)))
              when error
                do (return error))
        (and chunked
             (send stream (format nil "0~C~C~C~C" #\Return #\Newline #\Return #\Newline))))))

(defun receive (stream)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (values-list
     (handler-case
         (loop for byte = (read-byte stream nil nil)
               while byte
               do (vector-push-extend byte bytes)
               finally (return (list (utf-8-bytes-to-string bytes) nil)))
       (error (e)
         (list (utf-8-bytes-to-string bytes) e))))))

(defun status-line-p (status response)
  (eql 0 (search (format nil "HTTP/1.1 ~D " status) response)))

(defun wait-until (predicate &key (timeout 3))
  (loop repeat (* timeout 10)
        until (funcall predicate)
        do (sleep 0.1))
  (funcall predicate))

(defun app (env)
  (let ((raw-body (getf env :raw-body))
        (size 0))
    (when (typep raw-body 'file-stream)
      (setf *body-file* (pathname raw-body)))
    (loop while (read-byte raw-body nil nil)
          do (incf size))
    `(200 (:content-type "text/plain") (,(princ-to-string size)))))

(defvar *raw-body* nil)

(defun invalid-response-app (env)
  (setf *raw-body* (getf env :raw-body))
  :invalid-response)

(deftest body-limit-tests
  (let ((clack.test:*clack-test-handler* :woo))
    (with-body-limits
      (testing-app "A body past the memory limit"
          #'app
        (let ((size (* 2 *memory-limit*)))
          (multiple-value-bind (body status)
              (dex:post (localhost)
                        :content (make-array size :element-type '(unsigned-byte 8)
                                                  :initial-element 97))
            (ok (eql status 200))
            (ok (equal body (princ-to-string size)) "The app reads the whole body")))
        (ok *body-file* "The body is buffered in a file")
        (ok (not (probe-file *body-file*)) "The file the body was buffered in is deleted"))

      (testing-app "A Content-Length past the disk limit"
          #'app
        (with-connection (stream)
          (send-request stream "Content-Length: 10485760")
          (let ((response (receive stream)))
            (ok (status-line-p 413 response))
            (ok (search "connection: close" (string-downcase response)))))
        (ok (eql (nth-value 1 (dex:get (localhost))) 200)
            "The server goes on answering"))

      (testing-app "A chunked body past the disk limit"
          #'app
        (let ((before (body-files)))
          (with-connection (stream)
            (send-request stream "Transfer-Encoding: chunked"
                          :body-size (1+ *disk-limit*) :chunked t)
            (ok (status-line-p 413 (receive stream))))
          (ok (wait-until (lambda () (null (new-body-files before))))
              "The file the body was buffered in is deleted"))
        (ok (eql (nth-value 1 (dex:get (localhost))) 200)
            "The server goes on answering"))

      (testing-app "A body the connection goes away in the middle of"
          #'app
        (let ((before (body-files)))
          (with-connection (stream)
            (send-request stream (format nil "Content-Length: ~D" *disk-limit*)
                          :body-size (* 2 *memory-limit*))
            (ok (wait-until (lambda () (new-body-files before)))
                "The body is buffered in a file"))
          (ok (wait-until (lambda () (null (new-body-files before))))
              "The file is deleted when the connection closes")))

      (let ((clack.test:*enable-debug* nil))
        (testing-app "A response Woo fails to handle"
            #'invalid-response-app
          (setf *raw-body* nil)
          (ok (eql (handler-case (nth-value 1 (dex:post (localhost) :content "body"))
                     (dex:http-request-failed (e) (dex:response-status e)))
                   500)
              "Woo answers 500")
          (ok (wait-until (lambda () (and *raw-body* (not (open-stream-p *raw-body*)))))
              "The body is closed"))))))
