#lang racket/base

(require
 (only-in web-server/http
          response/full)
 (only-in web-server/http/request-structs
          make-header)
 (only-in web-server/servlet-env
          serve/servlet))

(define fixed-response
  (response/full
   200                          ;; code
   (string->bytes/utf-8 "OK")   ;; message
   (current-seconds)            ;; timestamp in s
   #f                           ;; mime or #f
   (list (make-header           ;; list of headers
          (string->bytes/utf-8 "Server")
          (string->bytes/utf-8 "Racket")))
   (list                        ;; body: list of bytes
    (string->bytes/utf-8 "Hello world!\n"))))

;; hello: request? -> response?
(define (hello req)
  fixed-response)

(module+ main
  (serve/servlet
   hello
   #:port 5000
   #:command-line? #t
   #:servlet-regexp #rx""))
