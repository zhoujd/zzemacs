#!/bin/bash

install_deps() {
    echo "Install deps"
    sudo apt install -y python3-pip
    sudo apt install -y python3-venv
    sudo apt install -y python3-virtualenv
}

install_venv() {
    echo "Install emacs venv"
    python3 -m venv ~/.venv/emacs
}

install_deps
install_venv

echo "Setup Python venv emacs done"
