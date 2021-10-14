#!/bin/bash

#VERSION_STRING="89.0.4389.82-1"
#URL_BASE=https://dl.google.com/linux/chrome/deb/pool/main/g/google-chrome-stable
VERSION_STRING=current
URL_BASE=https://dl.google.com/linux/direct

PKG_NAME=google-chrome-stable_${VERSION_STRING}_amd64.deb
PKG_URL=$URL_BASE/$PKG

download() {
    echo "Download chrome"
    pushd ~/Downloads/
    wget $PKG_URL
    popd
}

install() {
    echo "Install chrome"
    pushd ~/Downloads/
    if [ -f $PKG_NAME ]; then
        sudo dpkg -i $PKG_NAME
    fi
    popd
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

echo "Chome install done"
