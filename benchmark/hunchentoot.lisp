;;
;; sbcl --load benchmark/hunchentoot.lisp
;;
;; http://localhost:5000/

(ql:quickload :hunchentoot)

(hunchentoot:define-easy-handler (say-hello :uri "/") ()
  "Hello, World!")

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                  :port 5000
                                  :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)
                                  :access-log-destination nil))
