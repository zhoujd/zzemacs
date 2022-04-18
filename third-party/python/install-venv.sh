#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
VENV_ROOT=$HOME/.venv
VENV_PATH=$VENV_ROOT/emacs
VENV_PARA=include-system-site-packages

echo "For python venv start ..."

install() {
    echo "venv install"
    mkdir -p $VENV_ROOT
    if [ -d $VENV_PATH ]; then
        echo "Path: $VENV_PATH already exists"
    else
        python3 -m venv $VENV_PATH
    fi
}

config() {
    echo "venv config"
    sed -i "s/$VENV_PARA.*/$VENV_PARA = true/g" $VENV_PATH/pyvenv.cfg
}

case $1 in
    install )
        install
        ;;
    config )
        config
        ;;
    * )
        echo "Usage: $(basename $0) {install|config}"
        ;;
esac

echo "For python venv end ..."
