#!/bin/bash

dep() {
    sudo apt install -y python3-pip
    sudo apt install -y python3-venv
    sudo apt install -y python3-virtualenv
    echo "Install dependence done"
}

emacs() {
    python3 -m venv ~/.venv/emacs
    echo "Install python venv emacs done"
}

usage() {
    app=$(basename $0)
    cat << EOF
$app {dep|-d|emacs|-e}
EOF
}

case $1 in
    dep|-d )
        dep
        ;;
    emacs|-e )
        emacs
        ;;
    * )
        usage
        ;;
esac
