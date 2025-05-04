#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

TARGET=$HOME/.local/bin
mkdir -p $TARGET
cp -fv $SCRIPT_ROOT/pylsp-wrapper $TARGET

echo "pylsp setup done"
