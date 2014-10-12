# Woo

An asynchronous HTTP server written in Common Lisp bases on [cl-async](http://orthecreedence.github.com/cl-async) and [fast-http](https://github.com/fukamachi/fast-http).

## Warning

This library is still under development and considered ALPHA quality. APIs are likely to change.

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

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
