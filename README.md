# Woo

[![Build Status](https://travis-ci.org/fukamachi/woo.svg?branch=master)](https://travis-ci.org/fukamachi/woo)

Woo is a fast non-blocking HTTP server built on top of [libev](http://software.schmorp.de/pkg/libev.html). Although Woo is written in Common Lisp, it aims to be the fastest web server written in any programming language.

## Warning

This software is still BETA quality.

## How fast?

![Benchmark graph](images/benchmark.png)

See [benchmark.md](benchmark.md) for the detail.

## Usage

### Start a server

```common-lisp
(ql:quickload :woo)

(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World"))))
```

### Start with Clack

```common-lisp
(ql:quickload :clack)

(clack:clackup
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :server :woo
  :use-default-middlewares nil)
```

### Cluster

```common-lisp
(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :worker-num 4)
```

## Signal handling

When the master process gets these signals, it kills worker processes and quits afterwards.

- QUIT: graceful shutdown, waits for all requests are finished.
- INT/TERM: shutdown immediately.

## Benchmarks

See [benchmark.md](benchmark.md).

## Installation

Woo has switched the backend from cl-async to libev after the latest Quicklisp dist release. If you're gonna run the benchmarks by your own, please use the latest one.

### Requirements

* UNIX (GNU Linux, Mac, \*BSD)
* SBCL
* [libev](http://libev.schmorp.de)

### Installing via Quicklisp

```common-lisp
(ql:quickload :woo)
```

## See Also

* [libev](http://software.schmorp.de/pkg/libev.html)
* [Clack](http://clacklisp.org/)
* [Wookie](http://wookie.beeets.com)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi & [contributors](https://github.com/fukamachi/woo/graphs/contributors)

## License

Licensed under the MIT License.
