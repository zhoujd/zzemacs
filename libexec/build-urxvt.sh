#!/bin/bash

## https://www.linuxfromscratch.org/blfs/view/svn/xsoft/rxvt-unicode.html
## http://dist.schmorp.de/rxvt-unicode/Attic/
## https://github.com/exg/rxvt-unicode

PKG_NAME="rxvt-unicode"
PKG_VER="9.30"
PKG_REV="1"
PKG_DESC="Terminal emulator"
PKG_CAT="Utility"
PKG_DEPS=""

depend() {
    sudo apt update -y
    sudo apt install -y libptytty-dev libperl-dev
    return 0
}

download() {
    # download the sources
    wget http://dist.schmorp.de/rxvt-unicode/Attic/$PKG_NAME-$PKG_VER.tar.bz2
    [ $? -ne 0 ] && return 1
    return 0
}

build() {
    # extract the sources
    tar -xjvf $PKG_NAME-$PKG_VER.tar.bz2
    [ $? -ne 0 ] && return 1

    pushd $PKG_NAME-$PKG_VER

    # configure the package
    CXXFLAGS="$CXXFLAGS" ./configure \
            $BASE_CONFIGURE_ARGS \
            --prefix=/usr \
            --enable-everything

    [ $? -ne 0 ] && return 1

    # build the package
    make -j $BUILD_THREADS
    [ $? -ne 0 ] && return 1

    popd
    return 0
}

install() {
    pushd $PKG_NAME-$PKG_VER
    # install the package
    sudo make install
    [ $? -ne 0 ] && return 1

    # create a symlink for compatibility with existing stuff that rely on rxvt
    sudo ln -sfvT /usr/bin/urxvt /usr/bin/rxvt

    # create a menu entry
    sudo mkdir -p /usr/share/applications
    sudo tee /usr/share/applications/rxvt-unicode.desktop <<EOF
[Desktop Entry]
Encoding=UTF-8
Name=Rxvt-unicode terminal emulator
Icon=urxvt_48x48.svg
Comment=Rxvt-unicode terminal emulator
Exec=urxvt
Terminal=false
Type=Application
Categories=Utility;TerminalEmulator;
GenericName=Rxvt-unicode terminal emulator
EOF
    sudo chmod 644 /usr/share/applications/rxvt-unicode.desktop
    popd
    return 0
}

usage() {
    app=$(basename $0)
    echo "$(basename $0) {depend|download|-d|build|-b|install|-i"
    return 0
}

case $1 in
    depend )
        depend
        ;;
    download|-d )
        download
        ;;
    build|-b )
        build
        ;;
    install|-i )
        install
        ;;
    * )
        usage
        ;;
esac
