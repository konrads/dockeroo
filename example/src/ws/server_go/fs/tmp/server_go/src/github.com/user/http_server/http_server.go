package main

import (
	"io"
	"net/http"
)

func handle_req(w http.ResponseWriter, r *http.Request) {
	io.WriteString(w, "ok!!!")
}

func main() {
	http.HandleFunc("/", handle_req)
	http.ListenAndServe(":8888", nil)
}
