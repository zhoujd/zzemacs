#!/bin/bash

VERSION_STRING="89.0.4389.82-1"

install() {
    pushd ~/Downloads/
    wget "https://dl.google.com/linux/chrome/deb/pool/main/g/google-chrome-stable/google-chrome-stable_${VERSION_STRING}_amd64.deb"
    sudo dpkg -i "google-chrome-stable_${VERSION_STRING}"
    rm -i "google-chrome-stable_${VERSION_STRING}"
    popd
}

install
