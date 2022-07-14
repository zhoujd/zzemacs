#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

deps() {
    echo "Install deps"
    sudo apt install -y cmake libtool libtool-bin
}

build() {
    echo "build emacs-libvterm"
    git clone https://github.com/akermu/emacs-libvterm
    pushd emacs-libvterm
    cmake .
    make
    cp -vf vterm-module.so $SCRIPT_ROOT
    popd

    echo "clean libvterm"
    rm -rf emacs-libvterm
}

deps
build

echo "Build done"
