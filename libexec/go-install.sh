#!/bin/bash

# https://go.dev/dl/go1.19.linux-amd64.tar.gz
VERSION_STRING=1.19
URL_BASE=https://go.dev/dl

PKG_NAME=go${VERSION_STRING}.linux-amd64.tar.gz
PKG_URL=${URL_BASE}/${PKG_NAME}

download() {
    echo "Download go"
    pushd ~/Downloads/
    wget $PKG_URL
    popd
}

install() {
    echo "Install go"
    pushd ~/Downloads/
    if [ -f $PKG_NAME ]; then
        sudo rm -rf /usr/local/go
        sudo tar xf $PKG_NAME -C /usr/local/
    fi
}

clean() {
    echo "Clean $PKG_NAME"
    pushd ~/Downloads/
    if [ -f $PKG_NAME ]; then
        rm -f $PKG_NAME
    fi
    popd
}

case $1 in
    download )
        download
        ;;
    install )
        download
        install
        clean
        ;;
    clean )
        clean
        ;;
    * )
        echo "Usage $(basename $0) {download|install|clean}"
        ;;
esac

echo "go install done"
