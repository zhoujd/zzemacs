#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

install_ack_single() {
    local ver=v3.5.0
    local target=/usr/local/bin
    echo "Download ack"
    curl https://beyondgrep.com/ack-${ver} > $SCRIPT_ROOT/ack
    chmod +x $SCRIPT_ROOT/ack

    echo "Install ack"
    sudo mv $SCRIPT_ROOT/ack $target
}

install_ack_pkg() {
    sudo apt install -y ack-grep
}

#install_ack_single
#install_ack_pkg

echo "For perl develop end ..."
