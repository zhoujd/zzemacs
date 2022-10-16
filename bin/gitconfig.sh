#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
GITCONFIG_ROOT=$(cd $SCRIPT_ROOT/../misc/gitconfig.d && pwd)
GITCONFIG_TARGET=~/.gitconfig.d
GITCONFIG_FILE=~/.gitconfig

install() {
    echo "Install $GITCONFIG_ROOT to $GITCONFIG_TARGET"
    ln -sfvT $GITCONFIG_ROOT $GITCONFIG_TARGET

    echo "Install $GITCONFIG_FILE"
    pushd $GITCONFIG_TARGET
    ./install.sh
    popd

    echo "Install done"
}

uninstall() {
    echo "Uninstall $GITCONFIG_TARGET"
    rm -f $GITCONFIG_TARGET

    echo "Uninstall $GITCONFIG_FILE"
    rm -f $GITCONFIG_FILE
    
    echo "Uninstall done"
}

usage() {
    echo "Usage: $(basename $0) {install|uninstall}"
}

case $1 in
    install )
        install
        ;;
    uninstall )
        uninstall
        ;;
    * )
        usage
        ;;
esac
