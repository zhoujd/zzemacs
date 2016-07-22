#!/bin/bash

## Install from Org website
#git clone https://git.gnome.org/browse/meld
#sudo ln -sf `pwd`/meld/bin/meld /usr/bin

## Install from zzmeld
if [ ! -d zzmeld ]; then
    git clone https://github.com/zhoujd/zzmeld.git
fi

pushd zzmeld

./install.sh

popd
