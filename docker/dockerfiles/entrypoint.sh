#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$HOME/zzemacs

setup_emacs() {
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
    local libvirtd_cmd=/usr/sbin/libvirtd
    if [ -x $libvirtd_cmd ]; then
        echo "Setup libvirtd ..."
        sudo $libvirtd_cmd -d
    fi
}

setup_sshd() {
    local sshd_cmd=/usr/sbin/sshd
    if [ -x $sshd_cmd ]; then
        echo "Setup sshd ..."
        sudo $sshd_cmd
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
