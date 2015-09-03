(in-package :cl-user)
(defpackage woo.worker
  (:use :cl)
  (:import-from :woo.ev
                :*evloop*)
  (:export :make-cluster
           :add-job-to-cluster))
(in-package :woo.worker)

(defparameter *worker* nil)

(defstruct (worker (:constructor %make-worker (queue evloop async process-fn)))
  queue
  evloop
  async
  process-fn)

(defun add-job (worker job)
  (sb-concurrency:enqueue job (worker-queue worker)))

(defun notify-new-job (worker)
  (lev:ev-async-send (worker-evloop worker) (worker-async worker)))

(cffi:defcallback worker-dequeue :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore evloop listener events))
  (loop with queue of-type sb-concurrency:queue = (worker-queue *worker*)
        for socket = (sb-concurrency:dequeue queue)
        while socket
        do (funcall (worker-process-fn *worker*) socket)))

(defun make-worker-thread (process-fn)
  (let (worker
        (worker-lock (bt:make-lock)))
    (bt:make-thread
     (lambda ()
       (bt:acquire-lock worker-lock)
       (let ((*worker* nil)
             (queue (sb-concurrency:make-queue))
             (async (cffi:foreign-alloc '(:struct lev:ev-async))))
         (unwind-protect
              (wev:with-event-loop ()
                (setf worker (%make-worker queue *evloop* async process-fn))
                (bt:release-lock worker-lock)
                (setf *worker* worker)
                (lev:ev-async-init async 'worker-dequeue)
                (lev:ev-async-start *evloop* async))
           (cffi:foreign-free async))))
     :name "woo-worker")
    (sleep 0.1)
    (bt:acquire-lock worker-lock)
    worker))

;; TODO: notify when a thread terminated
(defstruct (cluster (:constructor %make-cluster (workers)))
  (workers '()))

(defun add-job-to-cluster (cluster job)
  (let* ((workers (cluster-workers cluster))
         (worker (car workers)))
    (add-job worker job)
    (notify-new-job worker)
    (setf (cluster-workers cluster)
          (cdr workers))))

(defun make-cluster (worker-num process-fn)
  (let ((workers '()))
    (dotimes (i worker-num)
      (push (make-worker-thread process-fn) workers))
    (setf (cdr (last workers)) workers)
    (%make-cluster workers)))

(defun terminate-cluster (cluster)
  (cluster-workers cluster))
