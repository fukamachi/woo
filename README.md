# Woo

[![Build Status](https://travis-ci.org/fukamachi/woo.svg?branch=master)](https://travis-ci.org/fukamachi/woo)

Woo is a fast non-blocking HTTP server built on top of [libev](http://software.schmorp.de/pkg/libev.html). Although Woo is written in Common Lisp, it aims to be the fastest web server written in any programming language.

## Warning

This library is still under development and considered ALPHA quality.

## How fast?

![Benchmark graph](images/benchmark.png)

See [Benchmarks](#benchmarks) for the detail.

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
- INT: shutdown immediately.

## Benchmarks

Comparison of the server performance to return "Hello, World" for every requests. Here's the results of requests/sec scores.

![Benchmark Results](images/benchmark.png)

Here's the new graph when using multiple CPU cores:

![Benchmark Results (multicore)](images/benchmark-multicore.png)

All benchmarks were done with the below command of [wrk](https://github.com/wg/wrk).

```
wrk -c [10 or 100] -t 4 -d 10 http://127.0.0.1:5000
```

The benchmarking environment is:

* Sakura VPS 4GB (CPU: 4 Core / Memory: 4GB)
* Ubuntu 14.04.3 LTS (GNU/Linux 3.13.0-61-generic x86_64)
* wrk 4.0.0
* nginx 1.4.6
* Python 2.7.6
* PyPy 2.6.0
* Tornado 4.2.1
* Quicklisp 2015-08-04
* SBCL 1.2.14
* Node.js 0.12.7
* Go 1.2.1
* Ruby 2.1.6p336
* Unicorn 4.9.0
* libuv 1.7.0
* libev 4.15

### Wookie (Common Lisp)

```
$ benchmark/run-benchmark benchmark/wookie/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     5.91ms    5.18ms  70.90ms   95.25%
    Req/Sec   376.95     91.68   540.00     61.25%
  15020 requests in 10.01s, 1.09MB read
Requests/sec:   1500.30
Transfer/sec:    111.35KB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    70.19ms   16.91ms 217.79ms   88.88%
    Req/Sec   362.65    123.18   505.00     57.83%
  14363 requests in 10.03s, 1.04MB read
Requests/sec:   1432.61
Transfer/sec:    106.33KB
```

### Tornado (Python)

```
$ benchmark/run-benchmark python benchmark/tornado/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.82ms  191.20us  14.61ms   99.16%
    Req/Sec   415.18      9.05   430.00     75.00%
  16536 requests in 10.01s, 3.26MB read
Requests/sec:   1651.40
Transfer/sec:    333.83KB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    61.80ms    4.28ms 116.16ms   97.59%
    Req/Sec   404.76     67.47   505.00     67.50%
  16132 requests in 10.01s, 3.18MB read
Requests/sec:   1611.14
Transfer/sec:    325.69KB
```

### Hunchentoot (Common Lisp)

```
$ benchmark/run-benchmark benchmark/hunchentoot/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.87ms    9.71ms 154.74ms   98.20%
    Req/Sec   660.48     64.27   727.00     88.64%
  26122 requests in 10.01s, 4.38MB read
Requests/sec:   2609.58
Transfer/sec:    448.52KB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    39.67ms  103.29ms   1.64s    94.34%
    Req/Sec   517.47    339.19     1.27k    59.05%
  11368 requests in 10.03s, 1.91MB read
  Socket errors: connect 0, read 0, write 0, timeout 55
Requests/sec:   1133.89
Transfer/sec:    194.89KB
```

### Tornado (PyPy)

```
$ benchmark/run-benchmark pypy benchmark/tornado/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.73ms    2.98ms  55.99ms   98.95%
    Req/Sec     1.33k   112.68     1.44k    94.25%
  52836 requests in 10.01s, 10.43MB read
Requests/sec:   5275.79
Transfer/sec:      1.04MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    21.02ms    7.82ms 160.93ms   95.65%
    Req/Sec     1.23k   181.03     1.50k    85.50%
  48891 requests in 10.02s, 9.65MB read
Requests/sec:   4881.21
Transfer/sec:      0.96MB
```

### Node.js http module

```
$ benchmark/run-benchmark benchmark/node/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.54ms  327.61us  10.80ms   98.03%
    Req/Sec     1.29k    46.27     1.57k    80.00%
  51339 requests in 10.02s, 6.36MB read
Requests/sec:   5126.11
Transfer/sec:    650.78KB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    19.31ms    1.51ms  52.07ms   94.96%
    Req/Sec     1.30k    77.19     1.50k    89.75%
  51705 requests in 10.02s, 6.41MB read
Requests/sec:   5159.70
Transfer/sec:    655.04KB
```

### Woo (Common Lisp)

```
$ benchmark/run-benchmark benchmark/woo/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   811.53us    0.86ms  19.20ms   98.94%
    Req/Sec     2.57k   144.70     3.35k    89.58%
  102945 requests in 10.10s, 12.76MB read
Requests/sec:  10192.36
Transfer/sec:      1.26MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     9.29ms    7.86ms 236.64ms   98.21%
    Req/Sec     2.86k   326.20     4.99k    90.30%
  114613 requests in 10.10s, 14.21MB read
Requests/sec:  11347.52
Transfer/sec:      1.41MB
```

### Go

```
$ benchmark/run-benchmark benchmark/go/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   571.26us  779.80us  30.60ms   98.69%
    Req/Sec     3.52k   332.38     7.98k    82.04%
  140357 requests in 10.10s, 17.27MB read
Requests/sec:  13898.15
Transfer/sec:      1.71MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     7.20ms    4.12ms  76.59ms   70.88%
    Req/Sec     3.54k   244.72     4.27k    77.75%
  140821 requests in 10.01s, 17.32MB read
Requests/sec:  14069.35
Transfer/sec:      1.73MB
```

### Hunchentoot (multi-threaded-taskmaster)

```
$ benchmark/run-benchmark benchmark/hunchentoot/run true
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.41ms    3.68ms  42.62ms   95.47%
    Req/Sec     2.65k   306.72     2.93k    77.75%
  105501 requests in 10.00s, 15.80MB read
Requests/sec:  10548.24
Transfer/sec:      1.58MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    18.65ms   57.01ms 991.35ms   94.55%
    Req/Sec     2.57k     0.95k    6.34k    74.39%
  97717 requests in 10.07s, 14.63MB read
Requests/sec:   9703.66
Transfer/sec:      1.45MB
```

### Unicorn + nginx (Ruby, worker_processes=4)

nginx's worker\_processes=4
Unicorn's worker\_processes=4

```
$ sudo nginx -c benchmark/unicorn/nginx.conf
$ benchmark/run-benchmark benchmark/unicorn/run
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   775.04us  499.01us  13.46ms   89.84%
    Req/Sec     2.50k   408.49     3.88k    66.75%
  100194 requests in 10.10s, 16.81MB read
Requests/sec:   9921.11
Transfer/sec:      1.66MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.07ms    3.86ms 129.13ms   91.73%
    Req/Sec     4.04k   502.40     5.89k    69.00%
  161881 requests in 10.10s, 27.16MB read
Requests/sec:  16031.95
Transfer/sec:      2.69MB
```

### Node.js http module (4 cluster)

```
$ benchmark/run-benchmark benchmark/node/run 4
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   645.16us  411.18us  14.90ms   90.71%
    Req/Sec     3.01k   685.94     5.38k    61.69%
  120393 requests in 10.10s, 14.93MB read
Requests/sec:  11920.05
Transfer/sec:      1.48MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     8.15ms    4.40ms 149.51ms   98.43%
    Req/Sec     3.15k   121.25     3.38k    80.00%
  125509 requests in 10.01s, 15.56MB read
Requests/sec:  12540.72
Transfer/sec:      1.55MB
```

### Woo (Common Lisp, worker-num=4)

```
$ benchmark/run-benchmark benchmark/woo/run 4
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   502.66us  741.70us  22.38ms   99.37%
    Req/Sec     4.01k   397.81     4.57k    57.92%
  161072 requests in 10.10s, 19.97MB read
Requests/sec:  15948.20
Transfer/sec:      1.98MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.73ms    4.73ms 103.79ms   96.50%
    Req/Sec     5.79k   340.65     7.68k    76.50%
  230976 requests in 10.03s, 28.64MB read
Requests/sec:  23037.57
Transfer/sec:      2.86MB
```

### Go (GOMAXPROCS=4)

```
$ benchmark/run-benchmark benchmark/go/run 4
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   501.25us    0.98ms  19.31ms   96.17%
    Req/Sec     4.43k   531.76     6.43k    68.50%
  176648 requests in 10.02s, 21.73MB read
Requests/sec:  17636.86
Transfer/sec:      2.17MB
```

```
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.92ms    3.88ms  43.80ms   86.79%
    Req/Sec     6.98k     0.97k   10.55k    69.95%
  278553 requests in 10.05s, 34.27MB read
Requests/sec:  27722.19
Transfer/sec:      3.41MB
```

## Installation

Woo has switched the backend from cl-async to libev after the latest Quicklisp dist release. If you're gonna run the benchmarks by your own, please use the latest one.

### Requirements

* UNIX (GNU Linux, Mac, *BSD)
* SBCL
* Quicklisp
* [libev](http://libev.schmorp.de)

### Loading

```
(ql:quickload :woo)
```

## TODO

* SSL
* Make it faster, more scalable

## See Also

* [libev](http://software.schmorp.de/pkg/libev.html)
* [Wookie](http://wookie.beeets.com)
* [Clack](http://clacklisp.org/)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the MIT License.
