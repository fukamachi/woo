(in-package :cl-user)
(defpackage woo.queue
  (:use :cl)
  #+sbcl
  (:import-from :sb-concurrency
                :make-queue
                :queue-empty-p
                :enqueue
                :dequeue)
  (:export :make-queue
           :queue-empty-p
           :enqueue
           :dequeue))
(in-package :woo.queue)

#-sbcl
(progn
  (defstruct queue
    (raw-queue (cl-speedy-queue:make-queue 128))
    (lock (bt:make-lock)))

  (defun enqueue (object queue)
    (with-slots (raw-queue lock) queue
      (bt:with-lock-held (lock)
        (cl-speedy-queue:enqueue object raw-queue))))

  (defun dequeue (queue)
    (with-slots (raw-queue lock) queue
      (bt:with-lock-held (lock)
        (cl-speedy-queue:dequeue raw-queue))))

  (defun queue-empty-p (queue)
    (cl-speedy-queue:queue-empty-p (queue-raw-queue queue))))
