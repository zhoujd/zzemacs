#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/../.. && pwd)

setup_emacs() {
    echo "Setup emacs ..."
    ln -sfvT $ZZEMACS_ROOT/.emacs $HOME/.emacs
    mkdir -p $HOME/.fonts
    ln -sfvT $ZZEMACS_ROOT/font $HOME/.fonts/zach
    ln -sfvT $ZZEMACS_ROOT/etc/terminfo $HOME/.terminfo
}

setup_bash() {
    echo "Setup bashrc ..."
    $ZZEMACS_ROOT/bin/bashrc-setup.sh
}

setup_git() {
    echo "Setup git ..."
    $ZZEMACS_ROOT/misc/gitconfig.d/install-bin.sh
    $ZZEMACS_ROOT/misc/gitconfig.d/install-cfg.sh
    $ZZEMACS_ROOT/misc/gitconfig.d/install-token.sh
}

setup_emacs
setup_bash
setup_git
