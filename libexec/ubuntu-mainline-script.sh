#!/bin/bash

TARGET=/usr/local/bin

echo "Download script"
wget https://raw.githubusercontent.com/pimlie/ubuntu-mainline-kernel.sh/master/ubuntu-mainline-kernel.sh
chmod +x ubuntu-mainline-kernel.sh

echo "Install to $TARGET"
sudo mv ubuntu-mainline-kernel.sh $TARGET

echo "Done"
