#!/bin/bash

### wget https://ftp.gnu.org/gnu/emacs/emacs-25.2.tar.xz
### wget https://ftp.gnu.org/gnu/emacs/emacs-24.3.tar.xz

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

##Get script path
SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

EMACS_SRC=emacs-25.2
EMACS_SRC_FILE=${EMACS_SRC}.tar.xz

##Import vars and functions
. $SCRIPT_ROOT/sample.sh

echo "Build emacs begin ..."

Build_source()
{
    if [ ! -f $EMACS_SRC_FILE ]; then
        wget https://ftp.gnu.org/gnu/emacs/$EMACS_SRC_FILE
    fi

    tar xf $EMACS_SRC_FILE

    pushd $EMACS_SRC

    build_started=`date`
    echo "//Starting build: $build_started"
    
    ## compile emacs
    patch -p1 < $SCRIPT_ROOT/../misc/$EMACS_SRC.patch 
    autoconf
    ./configure --prefix=/usr
    make

    ## install emacs
    sudo make install

    build_finished=`date`
    echo
    echo "//build finished: $build_finished"

    popd
}

Install_libungif()
{
    wget https://sourceforge.net/projects/giflib/files/libungif-4.x/libungif-4.1.4/libungif-4.1.4.tar.gz
    tar xf libungif-4.1.4.tar.gz

    pushd libungif-4.1.4
    ./configure --prefix=/usr
    make
    sudo make install
    popd
}

Install_package()
{
    # dectect OS version
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper install libjpeg-devel libpng-devel giflib-devel libtiff-devel
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y automake
        sudo apt-get install -y build-essential
        sudo apt-get install -y libxft-dev libotf-dev
        sudo apt-get install -y libxpm-dev libpng12-dev libjpeg-dev libtiff5-dev libgif-dev
        sudo apt-get install -y libxaw7-dev libncurses5-dev libgtk2.0-dev librsvg2-dev libdbus-1-dev libgconf2-dev
        sudo apt-get install -y libotf-dev libgpm-dev libm17n-dev libgnutls-dev libselinux1-dev imagemagick
    elif [ "$OS_DISTRO" = "CentOS" ]; then
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
        sudo yum install -y gitk
    elif [ "$OS_DISTRO" = "Fedora" ]; then
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
        sudo dnf install -y gitk
        
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}

echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        run_cmd Install_package
        run_cmd Install_libungif
        ;;
esac

run_cmd Build_source

echo "Build emacs end ..."
