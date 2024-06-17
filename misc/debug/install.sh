#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

config() {
    ln -sfTv $SCRIPT_ROOT/gdbconf ~/.gdbconf
    echo "Install Config Done"
}

init() {
    cp -fv $SCRIPT_ROOT/gdbinit ~/.gdbinit
    echo "Install Init Done"
}

config
init

