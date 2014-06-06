#!/bin/sh

LINUX_DEV_TOOL_ROOT=`pwd`

##Import vars and functions
. $LINUX_DEV_TOOL_ROOT/sample.sh

echo "install linux-dev-tool begin..."

##package for ubuntu
Install_package_suse()
{
    sudo zypper install -y gcc gcc-c++ automake git gitk
	sudo zypper install -y ncurses-devel ncurses zlib-devel
	sudo zypper install -y tk tk-devel
	sudo zypper install -y flex cmake bison libtool perl-Archive-Zip
	sudo zypper install -y libpciaccess0-devel xorg-x11-devel libdrm-devel
	sudo zypper install -y libreoffice
}

##package for ubuntu
Install_package_ubuntu()
{
    sudo apt-get install libncurses5-dev libpthread-stubs0-dev libpciaccess-dev libxvmc-dev --yes
	sudo apt-get install xorg-dev autoconf libtool yasm --yes
	sudo apt-get install x11proto-dri2-dev --yes
	sudo apt-get install xutils-dev --yes
	sudo apt-get install g++ --yes
}

##package for centos
Install_package_centos()
{
    sudo yum install -y git gcc
    sudo yum install -y cmake gcc-c++ autoconf automake libtool libdrm-devel
    sudo yum install -y libX11-devel libpciaccess-devel xorg-x11-server-devel
}


# dectect OS version
if [ "$LINUX_DISTRO" = "SuSE" ]; then
    try_command Install_package_suse
elif [ "$LINUX_DISTRO" = "Ubuntu" ]; then
    try_command Install_package_ubuntu
elif [ "$LINUX_DISTRO" = "CentOS" ]; then
    try_command Install_package_centos
else
    echo "You are about to install on a non supported linux distribution."
fi

echo "install linux-dev-tool end..."
