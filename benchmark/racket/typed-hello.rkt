#lang typed/racket/base

(require/typed net/url-structs
  [#:struct path/param
   ([path : (U String (U 'up 'same))]
    [param : (Listof String)])]
  [#:struct url
   ([scheme : (U False String)]
    [user : (U False String)]
    [host : (U False String)]
    [port : (U False Exact-Nonnegative-Integer)]
    [path-absolute? : Boolean]
    [path : (Listof path/param)]
    [query : (Listof (Pairof Symbol (U False String)))]
    [fragment : (U False String)])])
(require/typed web-server/http
  [#:struct header
   ([field : Bytes]
    [value : Bytes])]
  [#:struct binding
   ([id : Bytes])]
  [#:struct request
   ([method : Bytes]
    [uri : url]
    [headers/raw : (Listof header)]
    [bindings/raw-promise : (Promise (Listof binding))]
    [post-data/raw : (U False Bytes)]
    [host-ip : String]
    [host-port : Number]
    [client-ip : String])]
  [#:struct response
   ([code : Number]
    [message : Bytes]
    [seconds : Number]
    [mime : (U False Bytes)]
    [headers : (Listof Bytes)]
    [output : (-> Boolean Any)])]
  [response/output (-> (-> Output-Port Void) response)])


(: hello (-> request response))
(define (hello req)
  (response/output (lambda (out) (display "Hello world!\n" out))))

(module+ main
  (require/typed web-server/servlet-env
    [serve/servlet (-> (-> request response)
                        (#:port Number)
                        (#:command-line? Boolean)
                        (#:servlet-regexp Regexp)
                        Void)])
  (serve/servlet
   hello
   #:port 5000
   #:command-line? #t
   #:servlet-regexp #rx""))
