#!/bin/sh

LINUX_TOOL_ROOT=`pwd`

##Import vars and functions
. $LINUX_TOOL_ROOT/sample.sh

echo "install linux-tool begin..."

##package for ubuntu
Install_package_suse()
{
    ##TODO
    true
}

##package for ubuntu
Install_package_ubuntu()
{
    sudo apt-get install -y texlive
    sudo apt-get install -y texinfo

    sudo apt-get install -y openssh-server

    ##easy split windows for multi-term
    sudo apt-get install -y terminator
}

##package for ubuntu
Install_package_centos()
{
    ##TODO
    true
}

# dectect OS version
if [ "$OS_DISTRO" = "SuSE" ]; then
    try_command Install_package_suse
elif [ "$OS_DISTRO" = "Ubuntu" ]; then
    try_command Install_package_ubuntu
elif [ "$OS_DISTRO" = "CentOS" ]; then
    try_command Install_package_centos
else
    echo "You are about to install on a non supported linux distribution."
fi

echo "install linux-tool end..."
