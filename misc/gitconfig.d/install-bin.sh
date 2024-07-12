#!/bin/bash

if [ "$OS" = "Windows_NT" ] ; then
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd -W)
else
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
fi

Install_bin() {
    local target=~/.local/bin
    if [ "$OS" = "Windows_NT" ] ; then
        echo "The script is support Git bash on Windows"
        ln -sfvT $SCRIPT_ROOT/bin/git-lmr /usr/bin/git-lmr
        ln -sfvT $SCRIPT_ROOT/bin/git-lpr /usr/bin/git-lpr
        ln -sfvT $SCRIPT_ROOT/bin/git-mr  /usr/bin/git-mr
        ln -sfvT $SCRIPT_ROOT/bin/git-pr  /usr/bin/git-pr
    else
        echo "Install git tools on Linux"
        mkdir -p $target
        ln -sfvT $SCRIPT_ROOT/bin/git-lmr $target/git-lmr
        ln -sfvT $SCRIPT_ROOT/bin/git-lpr $target/git-lpr
        ln -sfvT $SCRIPT_ROOT/bin/git-mr  $target/git-mr
        ln -sfvT $SCRIPT_ROOT/bin/git-pr  $target/git-pr
    fi
}

Install_bin
