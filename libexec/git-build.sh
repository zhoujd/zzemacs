#!/bin/bash

## sudo apt install autoconf build-essential tcl-dev libssl-dev libcurl4-openssl-dev gettext
## sudo yum install openssl-devel libcurl-devel expat-devel perl-devel

VER=2.26.0
PREFIX=/opt/git

build() {
    echo "build git from source"
    mkdir -p ~/Downloads
    pushd ~/Downloads
    wget https://github.com/git/git/archive/v${VER}.zip
    unzip v${VER}.zip
    cd git-${VER}
    make prefix=${PREFIX} all
    sudo make prefix=${PREFIX} install
    popd
}

build

echo "git build done"
