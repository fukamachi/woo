package main

// go build benchmark/hello.go

import (
	"fmt"
	"net/http"
	"runtime"
)

func hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, World")
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	http.HandleFunc("/", hello)
	http.ListenAndServe(":5000", nil)
}
