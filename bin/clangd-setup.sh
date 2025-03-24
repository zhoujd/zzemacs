#!/bin/bash
#set -x

dpkg -l | grep clangd || sudo apt install -y clangd
TARGET=$HOME/.local/bin
mkdir -p .local/bin
cp -fv clangd-wrapper $TARGET

echo "ccls setup done"
