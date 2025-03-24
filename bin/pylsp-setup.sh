#!/bin/bash
#set -x

TARGET=$HOME/.local/bin
mkdir -p .local/bin
cp -fv pylsp-wrapper $TARGET

echo "pylsp setup done"
