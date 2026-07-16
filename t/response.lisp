(in-package :cl-user)
(defpackage woo-test.response
  (:use :cl
        :rove))
(in-package :woo-test.response)

;;; Unit tests for the response status-line table.
;;;
;;; Two things are under test and they are easy to conflate:
;;;
;;;   `status-code-to-text'  -- a pure code -> reason-phrase function.
;;;   `*status-line*'        -- a hash table of code -> encoded status-line
;;;                             bytes, precomputed at load time by looping
;;;                             over a fixed code range.
;;;
;;; A code can be present in the first and absent from the second if it falls
;;; outside that loop's bounds. That is a real bug this file guards against:
;;; 511 was unreachable because the loop stopped at 510.

(defparameter *loop-lower-bound* 100)
(defparameter *loop-upper-bound* 511
  "Upper bound of the range `*status-line*' is built over, inclusive.
Must track the loop in src/response.lisp.")

(defun status-line-string (code)
  "Decoded status line for CODE, or NIL if not registered."
  (let ((bytes (gethash code woo.response::*status-line*)))
    (when bytes
      (map 'string #'code-char bytes))))

(defun reason-phrase (code)
  (woo.response::status-code-to-text code))

;;; ---------------------------------------------------------------------------

(deftest status-code-to-text-tests
  (testing "returns the reason phrase for known codes"
    (ok (equal (reason-phrase 200) "OK"))
    (ok (equal (reason-phrase 404) "Not Found"))
    (ok (equal (reason-phrase 500) "Internal Server Error")))

  (testing "returns NIL for unassigned codes"
    ;; 306 is reserved/unused; the others are simply not assigned.
    (ok (null (reason-phrase 306)))
    (ok (null (reason-phrase 419)))
    (ok (null (reason-phrase 499)))
    (ok (null (reason-phrase 599))))

  (testing "returns NIL outside the HTTP status range"
    (ok (null (reason-phrase 99)))
    (ok (null (reason-phrase 600)))))

(deftest status-line-format-tests
  (testing "a status line is HTTP/1.1 SP code SP reason CRLF"
    (ok (equal (status-line-string 200)
               (format nil "HTTP/1.1 200 OK~C~C" #\Return #\Linefeed)))
    (ok (equal (status-line-string 404)
               (format nil "HTTP/1.1 404 Not Found~C~C" #\Return #\Linefeed))))

  (testing "every registered status line is well formed"
    (let ((malformed '()))
      (maphash (lambda (code bytes)
                 (let ((line (map 'string #'code-char bytes)))
                   (unless (equal line
                                  (format nil "HTTP/1.1 ~D ~A~C~C"
                                          code (reason-phrase code)
                                          #\Return #\Linefeed))
                     (push code malformed))))
               woo.response::*status-line*)
      (ok (null malformed)
          (format nil "malformed status lines for: ~S" malformed))))

  (testing "status lines are octet vectors, not strings"
    ;; The write path runs under (safety 0) and passes these straight to
    ;; write-socket-data; a non-octet vector would corrupt output rather
    ;; than signal.
    (let ((wrong-type '()))
      (maphash (lambda (code bytes)
                 (unless (typep bytes '(simple-array (unsigned-byte 8) (*)))
                   (push code wrong-type)))
               woo.response::*status-line*)
      (ok (null wrong-type)
          (format nil "non-octet status lines for: ~S" wrong-type)))))

(deftest status-line-table-consistency-tests
  ;; This is the structural test: it ties the two representations together.
  ;; It fails if `status-code-to-text' knows a code that the precomputed
  ;; table does not -- which is exactly how 511 was broken.
  (testing "every code with a reason phrase is registered in *status-line*"
    (let ((unregistered '()))
      (loop for code from *loop-lower-bound* to *loop-upper-bound*
            when (and (reason-phrase code)
                      (null (gethash code woo.response::*status-line*)))
              do (push code unregistered))
      (ok (null unregistered)
          (format nil "codes with a reason phrase but no status line: ~S"
                  unregistered))))

  (testing "every registered code has a reason phrase"
    (let ((phraseless '()))
      (maphash (lambda (code bytes)
                 (declare (ignore bytes))
                 (unless (reason-phrase code)
                   (push code phraseless)))
               woo.response::*status-line*)
      (ok (null phraseless)
          (format nil "registered codes with no reason phrase: ~S" phraseless))))

  (testing "no reason phrase exists beyond the loop's upper bound"
    ;; If this fails someone added a code above the bound and it is
    ;; silently unreachable -- widen the loop in src/response.lisp.
    (let ((beyond '()))
      (loop for code from (1+ *loop-upper-bound*) to 599
            when (reason-phrase code)
              do (push code beyond))
      (ok (null beyond)
          (format nil "reason phrases outside the built range: ~S" beyond)))))

(deftest rfc-6585-and-8470-status-tests
  ;; Regression tests for the codes that were missing.
  ;; 429 is deliberately not covered here: it is added by PR #127, which is
  ;; still open. Once that lands 429 belongs in these lists.
  (testing "RFC 6585 codes are present"
    (ok (equal (reason-phrase 428) "Precondition Required"))
    (ok (equal (reason-phrase 431) "Request Header Fields Too Large"))
    (ok (equal (reason-phrase 511) "Network Authentication Required")))

  (testing "RFC 8470 425 Too Early is present"
    (ok (equal (reason-phrase 425) "Too Early")))

  (testing "they render as complete status lines"
    (dolist (code '(425 428 431 511))
      (ok (status-line-string code)
          (format nil "~D has a status line" code))))

  (testing "511 specifically, as it sits on the loop's upper bound"
    (ok (equal (status-line-string 511)
               (format nil "HTTP/1.1 511 Network Authentication Required~C~C"
                       #\Return #\Linefeed)))))

(deftest common-status-coverage-tests
  (testing "the codes a typical app returns are all registered"
    (dolist (code '(200 201 204 206
                    301 302 303 304 307 308
                    400 401 403 404 405 409 410 413 415 422
                    500 501 502 503 504))
      (ok (status-line-string code)
          (format nil "~D is registered" code)))))
