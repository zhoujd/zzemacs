#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
GITCONIF_ROOT=$(cd $SCRIPT_ROOT/../misc/gitconfig.d && pwd)
GITCONFI_TGT=~/.gitconfig.d

install() {
    echo "Install $GITCONIF_ROOT to $GITCONFI_TGT"
    ln -sfvT $GITCONIF_ROOT $GITCONFI_TGT

    echo "Install gitconfig"
    pushd $GITCONFI_TGT
    ./install.sh
    popd

    echo "Install done"
}

uninstall() {
    echo "Uninstall $GITCONFI_TGT"
    rm -f $GITCONFI_TGT
    rm -f ~/.gitconfig
    
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
