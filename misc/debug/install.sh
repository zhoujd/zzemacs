#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

install() {
    local target_dir=~/.gdbinit.d
    rm -rf $target_dir
    mkdir -p $target_dir
    cp -fv $SCRIPT_ROOT/gdbinit.d/* $target_dir
    cp -fv $SCRIPT_ROOT/gdbinit ~/.gdbinit
    echo "Install Done"
}

install
