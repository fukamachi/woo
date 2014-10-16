(in-package :cl-user)
(defpackage woo.url
  (:use :cl)
  (:import-from :fast-http.byte-vector
                :digit-byte-char-p
                :digit-byte-char-to-integer)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-byte)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:export :url-decode
           :parse-url))
(in-package :woo.url)

(define-condition url-decoding-error (simple-error) ())

(defun url-decode (path-bytes start end)
  (declare (type (simple-array (unsigned-byte 8) (*)) path-bytes))
  (let* ((p start)
         (byte (aref path-bytes p))
         (parsing-encoded-part nil))
    (handler-case
        (with-fast-output (buffer :vector)
          (macrolet ((go-state (tag)
                         `(progn
                            (incf p)
                            (when (= p end)
                              (go exit))
                            (setq byte (aref path-bytes p))
                            (go ,tag))))
            (tagbody
             start
               (cond
                 ((= byte #.(char-code #\%))
                  (go-state parse-encoded-part))
                 ((= byte #.(char-code #\+))
                  (fast-write-byte #.(char-code #\Space) buffer))
                 (T (fast-write-byte byte buffer)))
               (go-state start)

             parse-encoded-part
               (setq parsing-encoded-part
                     (* 16 (cond
                             ((digit-byte-char-p byte)
                              (digit-byte-char-to-integer byte))
                             ((<= 65 byte 69)
                              (- byte 55))
                             (T (error 'url-decoding-error)))))
               (go-state parse-encoded-part-second)

             parse-encoded-part-second
               (fast-write-byte
                (+ parsing-encoded-part
                   (cond
                     ((digit-byte-char-p byte)
                      (digit-byte-char-to-integer byte))
                     ((<= 65 byte 69)
                      (- byte 55))
                     (T (error 'url-decoding-error))))
                buffer)
               (setq parsing-encoded-part nil)
               (go-state start)

             exit
               (when parsing-encoded-part ;; EOF
                 (error 'url-decoding-error)))))
      (url-decoding-error ()
        (return-from url-decode path-bytes)))))

(defun parse-url (url-bytes start end)
  (declare (type (simple-array (unsigned-byte 8) (*)) url-bytes))
  (flet ((parse-path (bytes start)
           (values start
                   (or (position #.(char-code #\?) bytes :start start)
                       end)))
         (parse-query (bytes start)
           (values start
                   (or (position #.(char-code #\#) bytes :start start)
                       end))))
    (if (= (aref url-bytes start) #.(char-code #\/))
        (multiple-value-bind (path-start path-end)
            (parse-path url-bytes start)
          (if (= path-end end)
              (values path-start path-end)
              (multiple-value-bind (query-start query-end)
                  (parse-query url-bytes (1+ path-end))
                (values path-start path-end query-start query-end))))
        (let ((protocol-end (search #.(trivial-utf-8:string-to-utf-8-bytes "://") url-bytes :start2 1)))
          (unless protocol-end
            (return-from parse-url))
          (incf protocol-end 3)
          (when (<= end protocol-end)
            (return-from parse-url))
          (let ((path-start (position #.(char-code #\/) url-bytes :start protocol-end)))
            (unless path-start
              (return-from parse-url))
            (parse-url url-bytes path-start end))))))
