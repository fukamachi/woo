(in-package :cl-user)
(defpackage woo.url
  (:use :cl)
  (:import-from :quri
                :parse-scheme
                :parse-authority
                :parse-path
                :parse-query)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:export :parse-url))
(in-package :woo.url)

(defun parse-url (url-bytes start end)
  (declare (type (simple-array (unsigned-byte 8) (*)) url-bytes))
  (multiple-value-bind (bytes scheme-start scheme-end)
      (parse-scheme url-bytes :start start :end end)
    (declare (ignore scheme-start))
    (multiple-value-bind (bytes path-start path-end)
        (if bytes
            (multiple-value-bind (bytes s e)
                (parse-authority url-bytes :start (1+ scheme-end) :end end)
              (declare (ignore s))
              (if bytes
                  (parse-path url-bytes :start e :end end)
                  (parse-path url-bytes :start (1+ scheme-end) :end end)))
            (parse-path url-bytes :start start :end end))
      (declare (ignore bytes))
      (multiple-value-bind (bytes query-start query-end)
          (parse-query url-bytes :start path-start :end end)
        (declare (ignore bytes))
        (values path-start path-end query-start query-end)))))
