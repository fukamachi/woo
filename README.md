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

* MacBook Pro Retina, 13-inch, Early 2013 (CPU: 3GHz Intel Core i7 / Memory: 8GB 1600 MHz)
* wrk 3.1.1
* SBCL 1.2.6
* Python 2.7.8
* PyPy 2.4.0
* Go 1.3.3
* Tornado 4.0.2
* Ruby 2.1.1p76
* Unicorn 4.8.3
* Phusion Passenger 5.0.0.beta2
* Node.js 0.10.21
* Quicklisp 2014-11-06
* libevent 2.1.4-alpha (HEAD)
* libev 4.15
* nginx 1.6.2

### Tornado (Python)

```
$ python benchmark/hello.py
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.83ms  218.41us   6.63ms   96.11%
    Req/Sec   735.93     83.69     0.89k    80.07%
  28223 requests in 10.00s, 5.57MB read
Requests/sec:   2822.09
Transfer/sec:    570.48KB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    39.14ms    1.02ms  45.78ms   73.21%
    Req/Sec   641.91     37.37   825.00     82.48%
  25504 requests in 10.01s, 5.03MB read
Requests/sec:   2548.72
Transfer/sec:    515.22KB
```

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

### Tornado (PyPy)

```
$ pypy benchmark/hello.py
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     4.21ms    8.64ms  30.26ms   90.41%
    Req/Sec     1.80k     1.11k    3.67k    56.02%
  67132 requests in 10.00s, 13.25MB read
Requests/sec:   6712.27
Transfer/sec:      1.33MB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    16.08ms    8.16ms  40.17ms   89.97%
    Req/Sec     1.60k   311.97     2.16k    63.06%
  62962 requests in 10.01s, 12.43MB read
Requests/sec:   6292.83
Transfer/sec:      1.24MB
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

### Unicorn + nginx (Ruby, worker_processes=4)

nginx's worker\_processes=4
Unicorn's worker\_processes=4

```
$ unicorn -E production -c benchmark/unicorn/config.rb benchmark/unicorn/app.ru
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.56ms    9.56ms 108.21ms   98.77%
    Req/Sec     4.09k     1.40k    9.22k    74.49%
  153585 requests in 10.00s, 24.45MB read
Requests/sec:  15361.96
Transfer/sec:      2.45MB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     6.60ms    4.55ms 137.92ms   89.16%
    Req/Sec     3.96k   657.47    11.31k    81.94%
  152428 requests in 10.00s, 24.27MB read
Requests/sec:  15243.51
Transfer/sec:      2.43MB
```

### Node.js http module (4 cluster)

```
$ node benchmark/node-cluster.js
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   334.44us  570.21us  19.06ms   90.08%
    Req/Sec     7.46k     5.29k   20.44k    63.69%
  280336 requests in 10.00s, 34.76MB read
Requests/sec:  28035.25
Transfer/sec:      3.48MB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.85ms    5.98ms 108.27ms   95.20%
    Req/Sec     7.73k     2.65k   31.56k    74.38%
  285013 requests in 10.00s, 35.34MB read
Requests/sec:  28498.97
Transfer/sec:      3.53MB
```

### Woo (Common Lisp, worker-num=4)

```
$ sbcl --load benchmark/woo-cluster.lisp
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.08ms   12.66ms 184.39ms   99.53%
    Req/Sec     9.86k     2.25k   15.00k    68.51%
  373207 requests in 10.00s, 46.98MB read
Requests/sec:  37323.22
Transfer/sec:      4.70MB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.86ms    9.19ms 126.45ms   96.01%
    Req/Sec    13.39k     6.71k   45.78k    67.70%
  504881 requests in 10.00s, 63.56MB read
Requests/sec:  50505.20
Transfer/sec:      6.36MB
```

### Go (GOMAXPROCS=4)

```
$ go build benchmark/hello-maxproc.go
$ ./hello-maxproc
```

```
$ wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   141.53us  140.85us   8.08ms   95.00%
    Req/Sec    13.50k     3.15k   24.44k    71.60%
  510975 requests in 10.00s, 62.86MB read
Requests/sec:  51101.54
Transfer/sec:      6.29MB
```

```
$ wrk -c 100 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.75ms    1.58ms  35.92ms   95.40%
    Req/Sec    15.55k     5.36k   62.67k    82.92%
  583243 requests in 10.00s, 71.75MB read
Requests/sec:  58336.29
Transfer/sec:      7.18MB
```

### Phusion Passenger (Ruby)

```
$ passenger start -e production -R benchmark/unicorn/app.ru --max-pool-size 4 --min-instances 4
```

```
wrk -c 10 -t 4 -d 10 http://127.0.0.1:3000
Running 10s test @ http://127.0.0.1:3000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   177.00us    1.10ms  45.88ms   99.64%
    Req/Sec    13.87k     3.06k   27.00k    80.29%
  526936 requests in 10.00s, 79.90MB read
Requests/sec:  52696.83
Transfer/sec:      7.99MB
```

```
wrk -c 100 -t 4 -d 10 http://127.0.0.1:3000
Running 10s test @ http://127.0.0.1:3000
  4 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.42ms    1.85ms  64.67ms   98.36%
    Req/Sec    15.60k     3.55k   47.00k    62.83%
  590815 requests in 10.00s, 89.59MB read
  Socket errors: connect 0, read 0, write 0, timeout 48
Requests/sec:  59085.92
Transfer/sec:      8.96MB
```

## Installation

Woo has switched the backend from cl-async to libev after the latest Quicklisp dist release. If you're gonna run the benchmarks by your own, please use the latest one.

### Requirements

* UNIX (GNU Linux, Mac, *BSD)
* SBCL 1.2.6
* Quicklisp
* [libev](http://libev.schmorp.de)

### Cloning

```
$ cd ~/common-lisp
$ git clone https://github.com/fukamachi/lev
$ git clone https://github.com/fukamachi/woo
```

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
