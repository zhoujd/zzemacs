#!/bin/bash

ZZEMACS_ROOT=$HOME/zzemacs

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

echo "Setup sshd"
sudo /usr/sbin/sshd -D
