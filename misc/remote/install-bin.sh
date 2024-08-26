#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
TARGET_DIR=/usr/local/bin

echo setup emacs remote bin start ...

sudo cp -vf $SCRIPT_ROOT/bin/ccls-wrapper $TARGET_DIR
sudo cp -vf $SCRIPT_ROOT/bin/clangd-wrapper $TARGET_DIR

echo setup emacs remote bin end ...
