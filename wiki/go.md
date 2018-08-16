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
