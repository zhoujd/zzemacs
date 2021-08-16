#!/bin/bash

echo "Init emacs go mode package"

go get golang.org/x/tools/cmd/godoc
go get golang.org/x/tools/cmd/goimports
go get github.com/rogpeppe/godef
go get github.com/nsf/gocode

echo "Emacs go pkg init done"
