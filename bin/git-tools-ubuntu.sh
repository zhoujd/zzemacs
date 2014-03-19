#!/bin/sh

GIT_SETUP_HOME=`pwd`

echo git diff tools for ubuntu setup start ...

## setup packages
if [ "$OS" != "Windows_NT" ] ; then
    echo -n "Do you need install packages? (y/N): "
    read answer
    case "$answer" in
        "Y" | "y" )
        sudo apt-get install -y python-nautilus python-configobj python-gtk2 python-glade2 python-svn python-dbus meld
        sudo apt-get install -y python-meld3
        sudo apt-get install -y git-core
        sudo apt-get install -y gitk
    esac
fi

echo git diff tools for ubuntu setup end ...

