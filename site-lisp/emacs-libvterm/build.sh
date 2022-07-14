#!/bin/bash

install_deps() {
    echo "Install deps"
    sudo apt install cmake
    sudo apt install libtool
    sudo apt install libtool-bin
}

install_deps

echo "Build done"
