(in-package :cl-user)
(defpackage woo-test.file-size
  (:use :cl
        :rove))
(in-package :woo-test.file-size)

;;; Regression test for a data race in `woo::fd-file-size'.
;;;
;;; The SBCL implementation used to fstat into one process-wide stat struct:
;;;
;;;   (defvar *stat* (make-instance 'sb-posix:stat))
;;;   (defun fd-file-size (fd)
;;;     (sb-posix:fstat fd *stat*)
;;;     (sb-posix:stat-size *stat*))
;;;
;;; Workers are threads (see woo.worker), not forked processes, so between
;;; fstat filling the struct and stat-size reading it another thread could
;;; overwrite it. The caller then got a different file's size, which becomes
;;; a wrong Content-Length on a sendfile response.
;;;
;;; Against the old code this test reported thousands of mismatches out of
;;; 80k calls; against the fix, zero.

#+sbcl
(defparameter *small-size* 1024)
#+sbcl
(defparameter *large-size* 786432)

#+sbcl
(defun write-file-of-size (path size)
  (with-open-file (out path :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
    (let ((buf (make-array (min size 65536)
                           :element-type '(unsigned-byte 8)
                           :initial-element 65)))
      (loop with remaining = size
            while (plusp remaining)
            for n = (min remaining (length buf))
            do (write-sequence buf out :end n)
               (decf remaining n))))
  path)

#+sbcl
(defun temp-path (name)
  (merge-pathnames name (uiop:temporary-directory)))

#+sbcl
(deftest fd-file-size-single-threaded-tests
  (let ((path (temp-path "woo-fd-file-size.dat")))
    (unwind-protect
         (progn
           (write-file-of-size path *small-size*)
           (let ((fd (sb-posix:open (namestring path) sb-posix:o-rdonly)))
             (unwind-protect
                  (testing "reports the true size of the file behind the fd"
                    (ok (= (woo::fd-file-size fd) *small-size*)))
               (sb-posix:close fd))))
      (ignore-errors (delete-file path)))))

#+sbcl
(deftest fd-file-size-concurrency-tests
  (testing "concurrent callers each get their own file's size"
    (let ((small (temp-path "woo-race-small.dat"))
          (large (temp-path "woo-race-large.dat")))
      (unwind-protect
           (progn
             (write-file-of-size small *small-size*)
             (write-file-of-size large *large-size*)
             (let ((small-fd (sb-posix:open (namestring small) sb-posix:o-rdonly))
                   (large-fd (sb-posix:open (namestring large) sb-posix:o-rdonly)))
               (unwind-protect
                    (let ((mismatches 0)
                          (sample nil)
                          (lock (bt2:make-lock))
                          (threads 8)
                          (iterations 10000))
                      (mapc
                       #'bt2:join-thread
                       (loop for i from 0 below threads
                             collect
                             (let* ((use-large (evenp i))
                                    (fd (if use-large large-fd small-fd))
                                    (expected (if use-large *large-size* *small-size*)))
                               (bt2:make-thread
                                (lambda ()
                                  (dotimes (k iterations)
                                    (let ((got (woo::fd-file-size fd)))
                                      (unless (= got expected)
                                        (bt2:with-lock-held (lock)
                                          (incf mismatches)
                                          (unless sample
                                            (setf sample
                                                  (list :expected expected
                                                        :got got))))))))
                                :name (format nil "woo-fd-size-racer-~D" i)))))
                      (ok (zerop mismatches)
                          (format nil "~D/~D concurrent fd-file-size calls returned another file's size~@[ (e.g. expected ~D, got ~D)~]"
                                  mismatches (* threads iterations)
                                  (getf sample :expected)
                                  (getf sample :got))))
                 (sb-posix:close small-fd)
                 (sb-posix:close large-fd))))
        (ignore-errors (delete-file small))
        (ignore-errors (delete-file large))))))
