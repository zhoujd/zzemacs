#!/bin/bash

font() {
    echo "Install console font"
    sudo cp -fv font/* /usr/share/consolefonts
    echo "Set Font"
    sudo setfont spleen-8x16.psfu.gz
    echo "Install font done"
}

conf() {
    sudo cp -fv console-setup /etc/default/
    sudo setupcon
    showconsolefont
    echo "Install config done"
}

case $1 in
    font|-f )
        font
        ;;
    conf|-c )
        conf
        ;;
    all|-a )
        font
        conf
        ;;
    * )
        echo "Usage: $(basename $0) {font|-f|conf|-c|all|-a}"
        ;;
esac
