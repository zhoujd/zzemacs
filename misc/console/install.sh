#!/bin/bash

install() {
    echo "Install console font"
    sudo cp -fv font/* /usr/share/consolefonts
    echo "Set Font"
    sudo setfont spleen-8x16.psfu.gz
    echo "Install font done"
}

config() {
    sudo cp -fv console-setup /etc/default/
    sudo setupcon
    showconsolefont
    echo "Config font done"
}

case $1 in
    install|-i )
        install
        ;;
    config|-c )
        config
        ;;
    * )
        echo "Usage: $(basename $0) {install|-i|config|-c}"
        ;;
esac
