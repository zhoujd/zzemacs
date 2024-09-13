#!/bin/bash
#set -x

dpkg -l | grep ccls || sudo apt install -y ccls
sudo cp -fv ccls-wrapper /usr/local/bin/

echo "ccls setup done"
