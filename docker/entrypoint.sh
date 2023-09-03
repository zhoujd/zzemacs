#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$HOME/zzemacs

setup_emacs() {
    if [ -d $ZZEMACS_ROOT ]; then
        echo "Setup emacs"
        ln -sfvT $ZZEMACS_ROOT/.emacs $HOME/.emacs
        echo "Setup fonts"
        ln -sfvT $ZZEMACS_ROOT/font $HOME/.fonts
        echo "Setup bashrc"
        $ZZEMACS_ROOT/bin/bashrc-setup.sh
        echo "Setup git"
        $ZZEMACS_ROOT/misc/gitconfig.d/install-bin.sh
        $ZZEMACS_ROOT/misc/gitconfig.d/install-cfg.sh
    fi
}

setup_sshd() {
    echo "Setup sshd"
    sudo /usr/sbin/sshd -D
}

case $1 in
    help )
        echo "$(basename $0) {help}"
        ;;
    * )
        setup_emacs
        setup_sshd
        ;;
esac
