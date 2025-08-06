#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

gdb() {
    local target_dir=~/.gdbinit.d
    rm -rf $target_dir
    mkdir -p $target_dir
    cp -fv $SCRIPT_ROOT/gdbinit.d/common.gdb $target_dir
    cp -fv $SCRIPT_ROOT/gdbinit.d/function.gdb $target_dir
    cp -fv $SCRIPT_ROOT/gdbinit.d/stl-views.gdb $target_dir
    cp -fv $SCRIPT_ROOT/gdbinit ~/.gdbinit
    echo "Install GDB Done"
}

cgdb() {
    local target_dir=~/.cgdb
    rm -rf $target_dir
    mkdir -p $target_dir
    cp -fv $SCRIPT_ROOT/cgdb/cgdbrc $target_dir
    echo "Install CGDB Done"
}

case $1 in
    gdb )
        gdb
        ;;
    cgdb )
        cgdb
        ;;
    all )
        gdb
        cgdb
        ;;
    * )
        echo "Usage: $(basename $0) {gdb|cgdb|all}"
        ;;
esac
