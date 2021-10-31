#!/bin/bash

## https://beyondgrep.com/install/
## https://beyondgrep.com/more-tools/

VER=v3.5.0
sudo wget -O /usr/local/bin/ack https://beyondgrep.com/ack-$VER
sudo chmod +x /usr/local/bin/ack

echo "Install ack to /usr/local/bin/ done"
