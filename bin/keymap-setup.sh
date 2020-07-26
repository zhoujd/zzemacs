#!/bin/bash

echo "Set capslk to ctrl"

### https://unix.stackexchange.com/questions/9098/map-windows-key-on-keyboard-to-ctrl
## grep ctrl /usr/share/X11/xkb/rules/evdev.lst

keymap_ubuntu() {
    echo "on Ubuntu"
    sudo sed -i 's/XKBOPTIONS=.*/XKBOPTIONS="ctrl:nocaps"/' /etc/default/keyboard
    sudo dpkg-reconfigure -phigh console-setup
}

keymap_ubuntu

echo "Set capslk to ctrl done"
