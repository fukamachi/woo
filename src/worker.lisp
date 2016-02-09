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

(defstruct worker
  (queue (make-queue))
  evloop
  dequeue-async
  stop-async
  process-fn)

(defun add-job (worker job)
  (enqueue job (worker-queue worker)))

(defun notify-new-job (worker)
  (lev:ev-async-send (worker-evloop worker) (worker-dequeue-async worker)))

(defun stop-worker (worker)
  (with-slots (evloop dequeue-async stop-async) worker
    (lev:ev-async-send evloop stop-async)
    (cffi:foreign-free dequeue-async)
    (cffi:foreign-free stop-async)
    (setf evloop nil
          dequeue-async nil
          stop-async nil)))

(cffi:defcallback worker-dequeue :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore evloop listener events))
  (loop with queue = (worker-queue *worker*)
        until (queue-empty-p queue)
        for socket = (dequeue queue)
        do (funcall (worker-process-fn *worker*) socket)))

(defparameter *timeout-retry* nil)
(cffi:defcallback worker-idle-checker :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore events))
  (if (or (= (hash-table-count wev:*data-registry*) 0)
          (= (decf *timeout-retry*) -1))
      (progn
        ;; Close existing all sockets.
        (maphash (lambda (fd socket)
                   (vom:warn "Force closing a socket (fd=~D)." fd)
                   (wev:close-socket socket))
                 wev:*data-registry*)

        ;; Stop all events.
        (lev:ev-break evloop lev:+EVBREAK-ALL+)
        (setf *timeout-retry* nil))
      (lev:ev-timer-again evloop listener)))

(defvar *stop-idle-timer* nil)
(cffi:defcallback worker-stop :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore listener events))

  ;; Wait until all requests are served or passed 10 sec.
  (unless *timeout-retry*
    (setf *timeout-retry* 10))
  (lev:ev-timer-init *stop-idle-timer* 'worker-idle-checker 1.0d0 0.0d0)
  (lev:ev-timer-start evloop *stop-idle-timer*))

(defun make-worker-thread (process-fn)
  (let* ((dequeue-async (cffi:foreign-alloc '(:struct lev:ev-async)))
         (stop-async (cffi:foreign-alloc '(:struct lev:ev-async)))
         (worker (make-worker :dequeue-async dequeue-async
                              :stop-async stop-async
                              :process-fn process-fn))
         (worker-lock (bt:make-lock)))
    (lev:ev-async-init dequeue-async 'worker-dequeue)
    (lev:ev-async-init stop-async 'worker-stop)
    (bt:make-thread
     (lambda ()
       (bt:acquire-lock worker-lock)
       (let ((*worker* worker)
             (*stop-idle-timer*  (cffi:foreign-alloc '(:struct lev:ev-timer))))
         (unwind-protect
              (wev:with-event-loop ()
                (setf (worker-evloop worker) *evloop*)
                (bt:release-lock worker-lock)
                (lev:ev-async-start *evloop* dequeue-async)
                (lev:ev-async-start *evloop* stop-async))
           (cffi:foreign-free *stop-idle-timer*))))
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

(defun stop-cluster (cluster)
  (let ((hash (make-hash-table :test 'eq)))
    (loop for worker in (cluster-workers cluster)
          if (gethash worker hash)
            do (return)
          else
            do (setf (gethash worker hash) t)
               (stop-worker worker))))
