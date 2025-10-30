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
    cmake . -DUSE_SYSTEM_LIBVTERM=NO
    make
    cp -vf vterm-module.so $SCRIPT_ROOT
    cp -vf vterm.el $SCRIPT_ROOT
    cp -vfr etc/* $SCRIPT_ROOT/etc/
    popd

    echo "clean emacs-libvterm"
    rm -rf emacs-libvterm

    echo "Build done"
}

case $1 in
    deps )
        deps
        ;;
    build )
        build
        ;;
    * )
        echo "Usage: $(basename $0) {deps|build}"
        ;;
esac
