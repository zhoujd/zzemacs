#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
VENV_ROOT=$HOME/.venv
VENV_PATH=$VENV_ROOT/emacs
VENV_CONF=include-system-site-packages

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
    sed -i "s/$VENV_CONF.*/$VENV_CONF = true/g" $VENV_PATH/pyvenv.cfg
}

usage() {
    local app=$(basename $0)
    cat <<EOF
Usage: $app {install|config|all}
EOF
}

case $1 in
    install )
        install
        ;;
    config )
        config
        ;;
    all )
        install
        ;;
    * )
        usage
        ;;
esac

echo "For python venv end ..."
