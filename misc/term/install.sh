#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

install() {
    ln -sfvT ${SCRIPT_ROOT}/terminfo ~/.terminfo
    echo "Install Done"
}

install
