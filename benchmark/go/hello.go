package main

import (
	"fmt"
	"net/http"
	"runtime"
	"flag"
)

func hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, World")
}

func main() {
	worker := flag.Int("worker", 1, "worker count")
	flag.Parse()
	runtime.GOMAXPROCS(*worker)

	http.HandleFunc("/", hello)
	http.ListenAndServe(":5000", nil)
}
