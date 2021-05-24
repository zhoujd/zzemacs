#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/.. && pwd)
TERMINFO_DIR=$ZZEMACS_HOME/etc/terminfo

if [ $# != 1 ]; then
    echo "Usage: $(basename $0) root@hostname"
    exit 1
fi

REMOTE=$1
terminfo_setup() {
    echo "setup remote terminfo"
    for i in $(ls $TERMINFO_DIR); do
        ssh $REMOTE mkdir -p /etc/terminfo/$i
        scp $TERMINFO_DIR/$i/* $REMOTE:/etc/terminfo/$i
        echo "remote copy $TERMINFO_DIR/$i done"
    done
}

ps1_setup() {
    echo "setup remote ps1"
    if [ -z $(ssh $REMOTE echo '$PS1') ]; then
        ssh $REMOTE echo "export PS1=$PS1 >> ~/.bashrc"
        echo "remote setup ps1 to $PS1 done"
    fi
}

terminfo-setup
ps1_setup

echo "emacs remote setup done"
