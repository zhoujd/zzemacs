#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

install() {
    ln -sfTv $SCRIPT_ROOT/gdbinit.d ~/.gdbinit.d
    cp -fv $SCRIPT_ROOT/gdbinit ~/.gdbinit
    echo "Install Done"
}

install
