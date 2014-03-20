#!/bin/sh

echo "install linux-tools begin..."

##package for ubuntu
Install_package_suse()
{
    ##TODO
}

##package for ubuntu
Install_package_ubuntu()
{
    #sudo apt-get install -y cairo-dock cairo-dock-plug-ins

    ##develop package
    #sudo apt-get install -y gnome-core-devel
    #sudo apt-get install -y acetoneiso

    sudo apt-get install -y libx11-dev
    sudo apt-get install -y xorg-dev
    sudo apt-get install -y texlive
    sudo apt-get install -y texinfo

    sudo apt-get install -y openssh-server

    ##easy split windows for multi-term
    sudo apt-get install -y terminator
}

# dectect OS version
LINUX_DISTRO=`lsb_release -si`
if [ "$LINUX_DISTRO" == "SUSE LINUX" ]; then
    LINUX_DISTRO="SuSE"
    Install_package_suse
elif [ "$LINUX_DISTRO" == "Ubuntu" ]; then
    LINUX_DISTRO="Ubuntu"
    Install_package_ubuntu
else
    echo "You are about to install on a non supported linux distribution."
fi

echo "Install on $LINUX_DISTRO ..."
echo "install linux-tools end..."
