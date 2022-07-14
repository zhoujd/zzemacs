#!/bin/bash

install_deps() {
    echo "Install deps"
    sudo apt install python3.8-venv
}

install_venv() {
    echo "Install emacs venv"
    python3 -m venv ~/.venv/emacs
}

install_deps
install_venv

echo "Setup Python venv emacs done"
