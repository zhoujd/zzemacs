#!/bin/bash

## https://github.com/garyparrot/rofi-translate
## https://github.com/soimort/translate-shell


install_trans() {
    echo "Install trans"
    local tmp=~/.tmp/trans
    mkdir -p ~/.tmp
    wget -O $tmp git.io/trans
    chmod +x $tmp

    sudo mv $tmp /usr/local/bin/
}

install_trans

echo "trans setup done ..."
