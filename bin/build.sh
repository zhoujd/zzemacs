#!/bin/sh

echo "Build emacs begin ..."

function Install_package
{
    # dectect OS version
    LINUX_DISTRO=`lsb_release -si`
    if [ "$LINUX_DISTRO" == "SUSE LINUX" ]; then
        LINUX_DISTRO="SuSE"
        sudo zypper install libjpeg-devel libpng-devel giflib-devel libtiff-devel
    elif [ "$LINUX_DISTRO" == "Ubuntu" ]; then
        LINUX_DISTRO="Ubuntu"
        sudo apt-get build-dep emacs23
        sudo apt-get install -y build-essential
        sudo apt-get install -y libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev
        sudo apt-get install -y libxaw7-dev libncurses5-dev libgtk2.0-dev librsvg2-dev libdbus-1-dev libgconf2-dev
        sudo apt-get install -y libotf-dev libgpm-dev libm17n-dev libgnutls-dev libselinux1-dev imagemagick
    else
        echo "You are about to install on a non supported linux distribution."
    fi

    echo "Install on $LINUX_DISTRO ..."
}

echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        Install_package
        ;;
esac

./configure
make
sudo make install

echo "Build emacs end ..."
