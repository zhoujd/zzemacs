GO Lang
=======

1. Debug with GDB

        <https://golang.org/doc>
        <https://golang.org/doc/gdb>

        $ go run main.go
        $ go build -gcflags "-N -l" -o gdb_sandbox main.go
        $ gdb gdb_sandbox

        (gdb) source ~/go/src/runtime/runtime-gdb.py
        or
        (gdb) source /usr/share/go/src/runtime/runtime-gdb.py

        (gdb) l
        (gdb) b main.go:9
        (gdb) r

2. Godoc

        $ sudo apt install golang-golang-x-tools

3. Go project directories

        <https://github.com/golang-standards/project-layout>

4. Go as script

        $ cat vikas.go
        //usr/bin/env go run "$0" "$@"; exit

        package main

        import "fmt"

        func main() {
            fmt.Printf("Hello World\n")
        }

        $ ./vikas.go
        Hello World

5. Init Go project

        $ mkdir -p api build cmd deployments docs internal pkg scripts test third-party tools vendor
        $ touch Makefile README.md

6. Ginkgo Go testing framework

        ## https://github.com/onsi/ginkgo
        $ go get -u github.com/onsi/ginkgo/ginkgo
