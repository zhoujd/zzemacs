#!/bin/bash
#set -x

dpkg -l | grep ccls || sudo apt install -y ccls
TARGET=$HOME/.local/bin
mkdir -p .local/bin
cp -fv ccls-wrapper $TARGET

echo "ccls setup done"
