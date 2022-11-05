#!/bin/bash

# 3.8  -> Ubuntu 20.04
# 3.10 -> Ubuntu 22.04
VER=3.10

install_deps() {
    echo "Install deps"
    sudo apt install python${VER}-venv
}

install_venv() {
    echo "Install emacs venv"
    python3 -m venv ~/.venv/emacs
}

install_deps
install_venv

echo "Setup Python venv emacs done"
