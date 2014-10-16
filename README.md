# Woo

Woo is a non-blocking HTTP server written in Common Lisp built on top of [cl-async](http://orthecreedence.github.com/cl-async) and [fast-http](https://github.com/fukamachi/fast-http).

The architecture is pretty similar to Wookie, except Woo adopts Clack-compatible APIs and delegates all "web-application-framework-like" parts to the Clack ecosystem.

## Warning

This library is still under development and considered ALPHA quality.

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

## Run in parallel (experimental)

Woo can work in parallel by specifying `:kernel-count` keyword argument to `woo:run`.

```common-lisp
(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :kernel-count 4)
```

## Benchmark

Comparison of the server performance to return "Hello, World" for every requests. Here's the results of requests/sec scores.

![Benchmark Results](images/benchmark.png)

I used the below command of [wrk](https://github.com/wg/wrk).

```
wrk -c 15 -t 5 -d 5 http://127.0.0.1:5000
```

The benchmarking environment is:

* MacBook Pro Retina, 13-inch, Early 2013 (CPU: 3GHz Intel Core i7 / Memory: 8GB 1600 MHz)
* wrk 3.1.1
* SBCL 1.2.4
* Node.js 0.10.21
* Ruby 2.1.1
* Quicklisp 2014-10-06
* libevent 2.1.4-alpha (HEAD)

NOTE: Though the machine has multiple CPUs, this benchmark assumes single core environment. Some web servers has features to run on multiple CPU cores with better performance, like Hunchentoot's threaded taskmanager or Node.js's cluster.

### Goliath (Ruby)

v1.0.4

```
$ RACK_ENV=production ruby benchmark/goliath.rb -v
```

```
Running 5s test @ http://127.0.0.1:9000
  5 threads and 15 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.22ms    1.01ms   9.75ms   75.84%
    Req/Sec     1.03k   245.69     1.67k    64.58%
  24180 requests in 5.00s, 2.42MB read
Requests/sec:   4834.73
Transfer/sec:    495.75KB
```

### Hunchentoot (Common Lisp)

```
$ sbcl --load benchmark/hunchentoot.lisp
```

```
Running 5s test @ http://127.0.0.1:5000
  5 threads and 15 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   150.77us   86.97us   3.83ms   99.61%
    Req/Sec     6.57k   612.06     7.33k    65.92%
  30260 requests in 6.00s, 4.53MB read
  Socket errors: connect 0, read 0, write 0, timeout 35
Requests/sec:   5042.76
Transfer/sec:    773.16KB
```

### Wookie (Common Lisp)

```
$ sbcl --load benchmark/wookie.lisp
```

```
Running 5s test @ http://127.0.0.1:5000
  5 threads and 15 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.38ms    3.60ms  41.96ms   98.18%
    Req/Sec     1.55k   308.14     2.00k    76.65%
  36453 requests in 5.00s, 4.00MB read
Requests/sec:   7293.43
Transfer/sec:    819.09KB
```

### Node.js http module

```
$ node benchmark/node.js
```

```
Running 5s test @ http://127.0.0.1:5000
  5 threads and 15 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     0.94ms  198.93us   6.11ms   70.59%
    Req/Sec     3.42k   588.23     4.89k    71.14%
  80950 requests in 5.00s, 10.04MB read
Requests/sec:  16193.35
Transfer/sec:      2.01MB
```

### Woo (Common Lisp)

```
$ sbcl --load benchmark/woo.lisp
```

```
Running 5s test @ http://127.0.0.1:5000
  5 threads and 15 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   644.06us  266.32us   7.97ms   97.68%
    Req/Sec     4.85k   507.15     5.78k    79.64%
  114923 requests in 5.00s, 5.59MB read
Requests/sec:  22987.92
Transfer/sec:      1.12MB
```

## Installation

```
git clone https://github.com/fukamachi/fast-http
git clone https://github.com/fukamachi/woo
```

```common-lisp
(ql:quickload :woo)
```

## TODO

* HTTP/1.0
* SSL
* Make it faster, more scalable

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
