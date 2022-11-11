#!/bin/bash

if [ "$OS" = "Windows_NT" ] ; then
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd -W)
else
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
fi

Install_bin() {
    echo "Install git tools"
    sudo ln -sfvT $SCRIPT_ROOT/bin/git-lmr /usr/bin/git-lmr
    sudo ln -sfvT $SCRIPT_ROOT/bin/git-lpr /usr/bin/git-lpr
    sudo ln -sfvT $SCRIPT_ROOT/bin/git-mr  /usr/bin/git-mr
    sudo ln -sfvT $SCRIPT_ROOT/bin/git-pr  /usr/bin/git-pr
}

Install_bin
