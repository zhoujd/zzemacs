#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

install() {
    local target=~/.terminfo
    mkdir -p $target
    cp -rfv ${SCRIPT_ROOT}/terminfo/{e,p,r,s,t} $target
    echo "Install Done"
}

install
