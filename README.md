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

## Benchmark

Comparison of the server performance to return "Hello, World" for every requests. Here's the results of requests/sec scores.

![Benchmark Results](images/benchmark.png)

I used the below command of [wrk](https://github.com/wg/wrk).

```
wrk -c [10 or 100] -t 4 -d 10 http://127.0.0.1:5000
```

The benchmarking environment is:

* MacBook Pro Retina, 13-inch, Early 2013 (CPU: 3GHz Intel Core i7 / Memory: 8GB 1600 MHz)
* wrk 3.1.1
* SBCL 1.2.6
* Node.js 0.10.21
* Quicklisp 2014-11-06
* libevent 2.1.4-alpha (HEAD)
* libev 4.15

NOTE: Though the machine has multiple CPUs, this benchmark assumes single core environment. Some web servers has features to run on multiple CPU cores with better performance, like Hunchentoot's threaded taskmanager or Node.js's cluster.

### Hunchentoot (Common Lisp)

```
$ sbcl --load benchmark/hunchentoot.lisp
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   144.00us   97.95us   4.76ms   99.85%
    Req/Sec     6.81k   509.00     7.22k    97.05%
  63978 requests in 10.01s, 9.58MB read
  Socket errors: connect 0, read 0, write 0, timeout 34
Requests/sec:   6390.08
Transfer/sec:      0.96MB
```

```
wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.24ms    2.45ms   5.19ms   61.52%
    Req/Sec     6.83k   556.53     7.89k    93.36%
  64140 requests in 10.01s, 9.60MB read
  Socket errors: connect 0, read 161, write 0, timeout 387
Requests/sec:   6406.92
Transfer/sec:      0.96MB
```

### Wookie (Common Lisp)

```
$ sbcl --load benchmark/wookie.lisp
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.25ms    3.35ms  52.53ms   99.16%
    Req/Sec     1.99k   227.28     2.33k    78.41%
  75538 requests in 10.00s, 8.28MB read
Requests/sec:   7554.20
Transfer/sec:    848.37KB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    13.15ms    1.89ms  25.52ms   85.12%
    Req/Sec     1.94k   349.30     2.59k    64.35%
  76227 requests in 10.01s, 8.36MB read
Requests/sec:   7618.58
Transfer/sec:    855.60KB
```

### Node.js http module

```
$ node benchmark/node.js
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   558.17us   57.01us   4.02ms   95.83%
    Req/Sec     3.73k   213.97     4.11k    63.56%
  141101 requests in 10.00s, 17.49MB read
Requests/sec:  14110.16
Transfer/sec:      1.75MB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     7.71ms    0.91ms  26.78ms   93.24%
    Req/Sec     3.37k   366.65     4.53k    74.85%
  130057 requests in 10.00s, 16.12MB read
Requests/sec:  13003.58
Transfer/sec:      1.61MB
```

### Woo (Common Lisp)

```
$ sbcl --load benchmark/woo.lisp
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   304.78us  435.89us  10.34ms   99.56%
    Req/Sec     7.06k     1.33k    9.89k    74.84%
  267269 requests in 10.00s, 33.65MB read
Requests/sec:  26728.29
Transfer/sec:      3.36MB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.83ms    1.35ms  23.13ms   95.43%
    Req/Sec     7.02k     1.03k    8.33k    82.96%
  266506 requests in 10.00s, 33.55MB read
Requests/sec:  26649.52
Transfer/sec:      3.35MB
```

### Go

```
$ go build benchmark/hello.go
$ ./hello
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   274.14us  142.62us   0.92ms   57.60%
    Req/Sec     7.47k     0.96k   10.60k    64.57%
  282965 requests in 10.00s, 34.81MB read
Requests/sec:  28298.26
Transfer/sec:      3.48MB
```

```
wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.72ms  742.47us   6.22ms   62.44%
    Req/Sec     7.09k   568.00     9.00k    65.49%
  269210 requests in 10.00s, 33.12MB read
Requests/sec:  26922.81
Transfer/sec:      3.31MB
```

## Installation

```
cd ~/common-lisp
git clone https://github.com/fukamachi/xsubseq
git clone https://github.com/fukamachi/fast-http
git clone https://github.com/fukamachi/quri
git clone https://github.com/fukamachi/woo
```

```common-lisp
(ql:quickload :woo)
```

## TODO

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
