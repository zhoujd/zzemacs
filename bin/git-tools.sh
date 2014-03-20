#!/bin/sh

function Install_package
{
    # dectect OS version
    LINUX_DISTRO=`lsb_release -si`
    if [ "$LINUX_DISTRO" == "SUSE LINUX" ]; then
        LINUX_DISTRO="SuSE"
        sudo zypper install git gitk
    elif [ "$LINUX_DISTRO" == "Ubuntu" ]; then
        LINUX_DISTRO="Ubuntu"
        sudo apt-get install -y python-nautilus python-configobj python-gtk2 python-glade2 python-svn python-dbus meld
        sudo apt-get install -y python-meld3
        sudo apt-get install -y git-core
        sudo apt-get install -y gitk
    else
        echo "You are about to install on a non supported linux distribution."
    fi
        
	echo "Install on $LINUX_DISTRO ..."

}

## setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        Install_package
        ;;
esac

echo "git diff tools setup end ..."

