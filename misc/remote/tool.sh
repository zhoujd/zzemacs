#!/bin/bash

source /etc/os-release

if [ "$ID" == "ubuntu" ]; then
    echo "Run on Ubuntu"
    sudo apt install -y silversearcher-ag cscope emacs-bin-common
elif [ "$ID" == "centos" ]; then
    echo "Run on CentOS"
    sudo yum install -y epel-release
    sudo yum install -y silversearcher-ag cscope emacs-bin-common
else
    echo "Unsupport OS: $ID"
fi

echo "Install tool done"
