#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

install_font() {
    target=~/.config/fontconfig/fonts.conf
    rm -fv $target
    cp -vf $SCRIPT_ROOT/fonts.conf $target
    fc-match monospace
    fc-match monospace:bold
    echo "Install font done"
}

install_font
