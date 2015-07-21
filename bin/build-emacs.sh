#!/bin/sh

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

##Get script path
BUILD_ROOT=$(cd $(dirname $0) && pwd)

##Import vars and functions
. $BUILD_ROOT/sample.sh

echo "Build emacs begin ..."

Install_package()
{
    # dectect OS version
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper install libjpeg-devel libpng-devel giflib-devel libtiff-devel
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get build-dep -y emacs23
        sudo apt-get install -y build-essential
        sudo apt-get install -y libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev
        sudo apt-get install -y libxaw7-dev libncurses5-dev libgtk2.0-dev librsvg2-dev libdbus-1-dev libgconf2-dev
        sudo apt-get install -y libotf-dev libgpm-dev libm17n-dev libgnutls-dev libselinux1-dev imagemagick
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum groupinstall -y "Development Tools"
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
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}

echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        try_command Install_package
        ;;
esac

./configure
make
sudo make install

echo "Build emacs end ..."
