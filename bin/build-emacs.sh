#!/bin/bash

### $ build-emacs.sh <version> <prefix>
### wget https://ftp.gnu.org/gnu/emacs/emacs-26.3.tar.xz

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

##Get script path
SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

EMACS_VER=${1:-26.3}
EMACS_SRC=emacs-${EMACS_VER}
EMACS_SRC_FILE=${EMACS_SRC}.tar.xz
EMACS_PREFIX={$2:-/usr/local/${EMACS_SRC}}

##Import vars and functions
. $SCRIPT_ROOT/sample.sh

echo "Build emacs begin ..."

build_source() {
    if [ ! -f $EMACS_SRC_FILE ]; then
        wget https://ftp.gnu.org/gnu/emacs/$EMACS_SRC_FILE
    fi

    tar xf $EMACS_SRC_FILE

    pushd $EMACS_SRC

    build_started=`date`
    echo "//Starting build: $build_started"

    ./configure --prefix=${EMACS_PREFIX}
    make

    ## install emacs
    sudo make install

    build_finished=`date`
    echo
    echo "//build finished: $build_finished"

    popd
}

install_package() {
    # dectect OS version
    case "$OS_DISTRO" in
        "SUSE" )
            sudo zypper install libjpeg-devel libpng-devel giflib-devel libtiff-devel
            ;;
        "Ubuntu" | "LinuxMint" )
            sudo apt-get install -y automake
            sudo apt-get install -y build-essential
            sudo apt-get install -y libxft-dev libotf-dev libgpm-dev imagemagick
            sudo apt-get install -y libxpm-dev libpng-dev libjpeg-dev libtiff-dev libgif-dev
            sudo apt-get install -y libxaw7-dev libncurses5-dev libgtk2.0-dev librsvg2-dev libgconf2-dev
            sudo apt-get install -y libm17n-dev libgnutls28-dev libselinux1-dev libdbus-1-dev
            ;;
        "CentOS" )
            sudo yum -y groupinstall "Development Tools"
            sudo yum install -y gtk+-devel gtk2-devel
            sudo yum install -y libXpm-devel
            sudo yum install -y libpng-devel
            sudo yum install -y giflib-devel
            sudo yum install -y libtiff-devel libjpeg-devel
            sudo yum install -y ncurses-devel
            sudo yum install -y gpm-devel dbus-devel dbus-glib-devel dbus-python
            sudo yum install -y GConf2-devel pkgconfig
            sudo yum install -y libXft-devel
            sudo yum install -y git-core
            sudo yum install -y gitk git-gui
            sudo yum install -y wget
            ;;
        "Fedora" )
            sudo dnf -y groupinstall "Development Tools"
            sudo dnf install -y gtk+-devel gtk2-devel
            sudo dnf install -y libXpm-devel
            sudo dnf install -y libpng-devel
            sudo dnf install -y giflib-devel
            sudo dnf install -y libtiff-devel libjpeg-devel
            sudo dnf install -y ncurses-devel
            sudo dnf install -y gpm-devel dbus-devel dbus-glib-devel dbus-python
            sudo dnf install -y GConf2-devel pkgconfig
            sudo dnf install -y libXft-devel
            sudo dnf install -y git-core
            sudo dnf install -y gitk git-gui
            ;;
        "Arch" | "Manjaro" )
            sudo pacman -S libjpeg libtiff giflib imagemagick
            ;;
        * )
            echo "You are about to install on a non supported linux distribution."
            ;;
    esac
}

echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        run_cmd install_package
        ;;
esac

run_cmd build_source

echo "Build emacs end ..."
