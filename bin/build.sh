#!/usr/bin/env bash

BUILD_ROOT=$HOME/zzemacs/bin
source $BUILD_ROOT/sample.sh

echo "Build emacs begin ..."

Install_package()
{
    # dectect OS version
    if [ "$LINUX_DISTRO" == "SuSE" ]; then
        sudo zypper install libjpeg-devel libpng-devel giflib-devel libtiff-devel
    elif [ "$LINUX_DISTRO" == "Ubuntu" ]; then
        sudo apt-get build-dep -y emacs23
        sudo apt-get install -y build-essential
        sudo apt-get install -y libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev
        sudo apt-get install -y libxaw7-dev libncurses5-dev libgtk2.0-dev librsvg2-dev libdbus-1-dev libgconf2-dev
        sudo apt-get install -y libotf-dev libgpm-dev libm17n-dev libgnutls-dev libselinux1-dev imagemagick
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
