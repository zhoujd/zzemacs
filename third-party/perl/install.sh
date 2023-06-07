#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

download_ack() {
    local ver=v3.5.0
    echo "Download ack"
    curl https://beyondgrep.com/ack-${ver} > $SCRIPT_ROOT/ack
    chmod +x $SCRIPT_ROOT/ack
}

install_ack() {
    echo "Install ack"
    sudo cp $SCRIPT_ROOT/ack /usr/local/bin/
}

#download_ack
#install_ack

echo "For perl develop end ..."
