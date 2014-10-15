;;
;; sbcl --load benchmark/woo.lisp --eval '(quit)'
;;
;; http://localhost:5000/

(ql:quickload :woo)

(format t "Running at http://localhost:5000/~%")

(woo:run (lambda (env)
           (declare (ignore env))
           '(200 () ("Hello, World")))
         :debug nil
         :use-thread nil
         :port 5000)
