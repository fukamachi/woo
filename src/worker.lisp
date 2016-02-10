(in-package :cl-user)
(defpackage woo.worker
  (:use :cl)
  (:import-from :woo.ev
                :*evloop*)
  (:import-from :woo.queue
                :make-queue
                :queue-empty-p
                :enqueue
                :dequeue)
  (:export :make-cluster
           :stop-cluster
           :add-job-to-cluster))
(in-package :woo.worker)

(defparameter *worker* nil)

(defvar *worker-counter* 0)

(defstruct worker
  (id (incf *worker-counter*))
  (queue (make-queue))
  evloop
  dequeue-async
  stop-async
  process-fn
  thread)

(defun add-job (worker job)
  (enqueue job (worker-queue worker)))

(defun notify-new-job (worker)
  (lev:ev-async-send (worker-evloop worker) (worker-dequeue-async worker)))

(defun stop-worker (worker)
  (format t "~&[~D] Stopping a worker...~%" (worker-id worker))
  (with-slots (evloop stop-async) worker
    (lev:ev-async-send evloop stop-async)))

(defun kill-worker (worker)
  (format t "~&[~D] Killing a worker...~%" (worker-id worker))
  (bt:destroy-thread (worker-thread worker)))

(cffi:defcallback worker-dequeue :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore evloop listener events))
  (loop with queue = (worker-queue *worker*)
        until (queue-empty-p queue)
        for socket = (dequeue queue)
        do (funcall (worker-process-fn *worker*) socket)))

(cffi:defcallback worker-stop :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore listener events))
  ;; Close existing all sockets.
  (maphash (lambda (fd socket)
             (wev:close-socket socket))
           wev:*data-registry*)

  ;; Stop all events.
  (lev:ev-break evloop lev:+EVBREAK-ALL+))

(defun finalize-worker (worker)
  (with-slots (evloop dequeue-async stop-async thread) worker
    (cffi:foreign-free dequeue-async)
    (cffi:foreign-free stop-async)
    (setf evloop nil
          dequeue-async nil
          stop-async nil
          thread nil)))

(defun make-worker-thread (process-fn)
  (let* ((dequeue-async (cffi:foreign-alloc '(:struct lev:ev-async)))
         (stop-async (cffi:foreign-alloc '(:struct lev:ev-async)))
         (worker (make-worker :dequeue-async dequeue-async
                              :stop-async stop-async
                              :process-fn process-fn))
         (worker-lock (bt:make-lock)))
    (lev:ev-async-init dequeue-async 'worker-dequeue)
    (lev:ev-async-init stop-async 'worker-stop)
    (setf (worker-thread worker)
          (bt:make-thread
           (lambda ()
             (bt:acquire-lock worker-lock)
             (let ((*worker* worker))
               (unwind-protect
                    (wev:with-event-loop ()
                      (setf (worker-evloop worker) *evloop*)
                      (bt:release-lock worker-lock)
                      (lev:ev-async-start *evloop* dequeue-async)
                      (lev:ev-async-start *evloop* stop-async))
                 (finalize-worker worker)
                 (format t "~&[~D] Bye.~%" (worker-id worker)))))
           :name "woo-worker"))
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

(defun stop-cluster (cluster)
  (let ((workers
          (loop with hash = (make-hash-table :test 'eq)
                for worker in (cluster-workers cluster)
                if (gethash worker hash)
                  do (return workers)
                else
                  collect
                  (progn (setf (gethash worker hash) t)
                         worker)
                  into workers)))
    (mapc #'stop-worker workers)
    (loop repeat 100
          while (find-if #'worker-thread workers)
          do (sleep 0.1)
          finally
             (mapc #'kill-worker (remove-if-not #'worker-thread workers)))))
