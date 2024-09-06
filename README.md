# Woo

[![CI](https://github.com/fukamachi/woo/actions/workflows/ci.yml/badge.svg)](https://github.com/fukamachi/woo/actions/workflows/ci.yml)

Woo is a fast non-blocking HTTP server built on top of [libev](http://software.schmorp.de/pkg/libev.html). Although Woo is written in Common Lisp, it aims to be the fastest web server written in any programming language.

## Warning

This software is still BETA quality.

## How fast?

![Benchmark graph](images/benchmark.png)

See [benchmark.md](benchmark.md) for the detail.

## Usage

Use `clack:clackup` or `woo:run` to start a web server. The first argument is a Lack "app". See [Lack's README](https://github.com/fukamachi/lack#readme) for instruction on how to build it.

Remember to pass ":debug nil" to turn off the debugger mode on production environments (it's on by default). Otherwise, your server will go down on internal errors.

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

### SSL Support

Use SSL key arguments of `woo:run` or `clack:clackup`.

```commonlisp
(woo:run app
         :ssl-cert-file #P"path/to/cert.pem"
         :ssl-key-file #P"path/to/key.pem"
         :ssl-key-password "password")

(clack:clackup app
               :ssl-cert-file #P"path/to/cert.pem"
               :ssl-key-file #P"path/to/key.pem"
               :ssl-key-password "password")
```

To disable the HTTPS support to omit a dependency on CL+SSL, add `woo-no-ssl` to `cl:*features*`.

## Signal handling

When the master process gets these signals, it kills worker processes and quits afterwards.

- QUIT: graceful shutdown, waits for all requests are finished.
- INT/TERM: shutdown immediately.

## Benchmarks

See [benchmark.md](benchmark.md).

## Installation

### Requirements

* UNIX (GNU Linux, Mac, \*BSD)
* SBCL
* [libev](http://libev.schmorp.de)
* OpenSSL or LibreSSL (Optional)
  * To turn off SSL, add `:woo-no-ssl` to `cl:*features*` before loading Woo.

### Installing via Quicklisp

```common-lisp
(ql:quickload :woo)
```

## Docker example

* [Dockerfile](https://github.com/quickdocs/quickdocs-api/blob/master/docker/Dockerfile.production) for Quickdocs's API server.

## See Also

* [Lack](https://github.com/fukamachi/lack): Building a web application
* [Clack](https://github.com/fukamachi/clack): An abstraction layer for web servers
* [libev](http://software.schmorp.de/pkg/libev.html)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi & [contributors](https://github.com/fukamachi/woo/graphs/contributors)

## License

Licensed under the MIT License.
