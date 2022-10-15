#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
GITCONIF_ROOT=$(cd $SCRIPT_ROOT/../misc/gitconf && pwd)
GITCONFI_TGT=~/.gitconfig.d

echo "Install $GITCONIF_ROOT to $GITCONFI_TGT"
ln -sfvT $GITCONIF_ROOT $GITCONFI_TGT

echo "Install gitconfig"
pushd $GITCONFI_TGT
./install.sh
popd

echo "Gitconfig done"
