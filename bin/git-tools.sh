#!/bin/sh

##Import vars and functions
. sample.sh

## Install package for git
Install_package()
{
    # dectect OS version
    if [ "$LINUX_DISTRO" = "SuSE" ]; then
        sudo zypper install git gitk
    elif [ "$LINUX_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y python-nautilus python-configobj python-gtk2 python-glade2 python-svn python-dbus meld
        sudo apt-get install -y python-meld3
        sudo apt-get install -y git-core
        sudo apt-get install -y gitk
    else
        echo "You are about to install on a non supported linux distribution."
    fi        
}

## setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        try_command Install_package
        ;;
esac

echo "git diff tools setup end ..."

