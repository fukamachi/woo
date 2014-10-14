# Woo

Woo is a non-blocking HTTP server written in Common Lisp built on top of [cl-async](http://orthecreedence.github.com/cl-async) and [fast-http](https://github.com/fukamachi/fast-http).

The architecture is pretty similar to Wookie, except Woo adopts Clack-compatible APIs and delegates most "web-application-framework-like" parts to the Clack ecosystem.

## Warning

This library is still under development and considered ALPHA quality.

## Usage

### Start a server

```common-lisp
(ql:quickload :woo)

(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain" ("Hello, World")))))
```

### Start with Clack

```common-lisp
(ql:quickload :clack)

(clack:clackup
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :server :woo)
```

## Installation

```
git clone https://github.com/fukamachi/fast-http
git clone https://github.com/fukamachi/woo
```

```common-lisp
(ql:quickload :woo)
```

## See Also

* [cl-async](http://orthecreedence.github.com/cl-async)
* [fast-http](https://github.com/fukamachi/fast-http)
* [Wookie](http://wookie.beeets.com)
* [Clack](http://clacklisp.org/)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
