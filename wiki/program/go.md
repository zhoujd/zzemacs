GO Lang
=======

## Debug with GDB

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

## Godoc

    $ sudo apt install golang-golang-x-tools

## Go project directories

     <https://github.com/golang-standards/project-layout>

## Go as script

    $ cat vikas.go
    //usr/bin/env go run "$0" "$@"; exit

    package main

    import "fmt"

    func main() {
        fmt.Printf("Hello World\n")
    }

    $ ./vikas.go
    Hello World

## Init Go project

    $ mkdir -p api build cmd deployments docs internal pkg scripts test third-party tools vendor
    $ touch Makefile README.md

## Ginkgo Go testing framework

    ## https://github.com/onsi/ginkgo
    $ go get -u github.com/onsi/ginkgo/ginkgo
    or
    $ go get github.com/onsi/ginkgo/ginkgo
    $ go get github.com/onsi/gomega/...

## Install latest go version

    ## in case there is old version of go is installed
    $ sudo apt-get purge golang*

    ## get latest version from https://golang.org/dl/
    $ tar_name="go1.16.3.linux-amd64.tar.gz"
    $ wget https://golang.org/dl/$tar_name
    $ tar -xvf $tar_name
    $ rm -f $tar_name
    $ sudo rm -rf /usr/local/go
    $ sudo mv go /usr/local

    ## Add following lines into `.bashrc` or `.zshrc`
    $ mkdir -p $HOME/go
    $ export GOPATH=$HOME/go
    $ export GOROOT=/usr/local/go
    $ export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
