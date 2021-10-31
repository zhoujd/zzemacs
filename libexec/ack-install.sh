#!/bin/bash

## https://beyondgrep.com/install/
## https://beyondgrep.com/more-tools/
## Ubuntu: sudo apt install ack-grep
## Python: pip install pss

VER=v3.5.0
sudo wget -O /usr/local/bin/ack https://beyondgrep.com/ack-$VER
sudo chmod +x /usr/local/bin/ack

echo "Install ack to /usr/local/bin/ done"
