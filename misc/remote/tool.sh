#!/bin/bash

source /etc/os-release

if [ "$ID" == "ubuntu" ]; then
    echo "Run on Ubuntu"
    sudo apt install -y silversearcher-ag cscope emacs-bin-common
    sudo apt install -y ccls clangd
elif [ "$ID" == "centos" ]; then
    echo "Run on CentOS"
    sudo yum install -y epel-release
    sudo yum install -y silversearcher-ag cscope emacs-bin-common
    sudo yum install -y ccls clangd
else
    echo "Unsupport OS: $ID"
fi

echo "Install tool done"
