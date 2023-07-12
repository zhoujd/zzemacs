#!/bin/bash

## https://beyondgrep.com/install/
## https://beyondgrep.com/more-tools/
## Ubuntu: sudo apt install ack-grep
## Python: pip install pss

VER=v3.5.0
PREFIX=/usr/bin

sudo wget -O $PREFIX/ack https://beyondgrep.com/ack-$VER
sudo chmod +x $PREFIX/ack

echo "Install ack to $PREFIX done"
