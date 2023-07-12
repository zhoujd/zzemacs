#!/bin/bash

## sudo yum install -y pcre-devel xz-devel
## sudo yum install automake

install() {
    mkdir -p ~/Downloads/
    pushd ~/Downloads/
    git clone https://github.com/ggreer/the_silver_searcher.git
    cd the_silver_searcher
    ./build.sh
    make
    sudo make install
    popd

    echo "Install ag Done"
}

install
