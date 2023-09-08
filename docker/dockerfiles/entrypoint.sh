#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

setup_emacs() {
    local ZZEMACS_ROOT=$HOME/zzemacs
    if [ -d $ZZEMACS_ROOT ]; then
        echo "Setup emacs ..."
        ln -sfvT $ZZEMACS_ROOT/.emacs $HOME/.emacs
        echo "Setup fonts ..."
        ln -sfvT $ZZEMACS_ROOT/font $HOME/.fonts
        echo "Setup bashrc ..."
        $ZZEMACS_ROOT/bin/bashrc-setup.sh
        echo "Setup git ..."
        $ZZEMACS_ROOT/misc/gitconfig.d/install-bin.sh
        $ZZEMACS_ROOT/misc/gitconfig.d/install-cfg.sh
    fi
}

setup_libvirtd() {
    local LIBVIRTD_CMD=/usr/sbin/libvirtd
    if [ -x $LIBVIRTD_CMD ]; then
        echo "Setup libvirtd ..."
        sudo $LIBVIRTD_CMD -d
    fi
}

setup_sshd() {
    local SSHD_CMD=/usr/sbin/sshd
    if [ -x $SSHD_CMD ]; then
        echo "Setup sshd ..."
        sudo $SSHD_CMD
    fi
}

setup_sleep() {
    echo "Setup sleep ..."
    sleep infinity
}

CMD=${1:-""}
case $CMD in
    init )
        setup_emacs
        setup_libvirtd
        setup_sshd
        setup_sleep
        ;;
    * )
        setup_emacs
        setup_sleep
        ;;
esac
