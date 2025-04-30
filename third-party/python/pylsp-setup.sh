#!/bin/bash
#set -x

TARGET=$HOME/.local/bin
mkdir -p $TARGET
cp -fv pylsp-wrapper $TARGET

echo "pylsp setup done"
