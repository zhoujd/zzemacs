#!/bin/sh

LINUX_DEV_TOOL_ROOT=`pwd`

##Import vars and functions
. $LINUX_DEV_TOOL_ROOT/sample.sh

echo "install linux-dev-tool begin..."

##package for ubuntu
Install_package_suse()
{
    sudo zypper install -y gcc gcc-c++ automake git gitk diffstat
    sudo zypper install -y ncurses-devel ncurses zlib-devel
    sudo zypper install -y tk tk-devel
    sudo zypper install -y flex cmake bison libtool perl-Archive-Zip
    sudo zypper install -y libpciaccess0-devel xorg-x11-devel libdrm-devel
    sudo zypper install -y libreoffice
}

##package for ubuntu
Install_package_ubuntu()
{
    sudo apt-get install -y libncurses5-dev libpthread-stubs0-dev libpciaccess-dev libxvmc-dev
    sudo apt-get install -y xorg-dev autoconf libtool yasm
    sudo apt-get install -y x11proto-dri2-dev
    sudo apt-get install -y xutils-dev
    sudo apt-get install -y g++
    sudo apt-get install -y git gitk diffstat
}

##package for centos
Install_package_centos()
{
    sudo yum install -y git gcc diffstat
    sudo yum install -y cmake gcc-c++ autoconf automake libtool libdrm-devel
    sudo yum install -y libX11-devel libpciaccess-devel xorg-x11-server-devel
}


# dectect OS version
if [ "$OS_DISTRO" = "SUSE" ]; then
    try_command Install_package_suse
elif [ "$OS_DISTRO" = "Ubuntu" ]; then
    try_command Install_package_ubuntu
elif [ "$OS_DISTRO" = "CentOS" ]; then
    try_command Install_package_centos
else
    echo "You are about to install on a non supported linux distribution."
fi

echo "install linux-dev-tool end..."
