#!/bin/bash

echo "Set capslk to ctrl"

keymap_ubuntu() {
    echo "on Ubuntu"
    sudo sed -i 's/XKBOPTIONS=.*/XKBOPTIONS="ctrl:nocaps"/' /etc/default/keyboard
    sudo dpkg-reconfigure -phigh console-setup
}

keymap_ubuntu

echo "Set capslk to ctrl done"
