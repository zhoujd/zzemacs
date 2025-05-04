#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

install_bin() {
    local target=~/.local/bin
    echo "Install git tools on Linux"
    mkdir -p $target
    ln -sfvT $SCRIPT_ROOT/bin/git-lmr $target/git-lmr
    ln -sfvT $SCRIPT_ROOT/bin/git-lpr $target/git-lpr
    ln -sfvT $SCRIPT_ROOT/bin/git-mr  $target/git-mr
    ln -sfvT $SCRIPT_ROOT/bin/git-pr  $target/git-pr
}

install_bin $@
