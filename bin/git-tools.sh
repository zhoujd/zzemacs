#!/bin/sh

## Git script path
GIT_TOOLS_ROOT="$(dirname $0)"
ZZEMACS_ROOT="$(cd $GIT_TOOLS_ROOT/.. && pwd)"

##Import vars and functions
. $GIT_TOOLS_ROOT/sample.sh

## Install package for git
Install_package()
{
    # dectect OS version
    if [ "$OS_DISTRO" = "SuSE" ]; then
        sudo zypper install git gitk
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y python-nautilus python-configobj python-gtk2 python-glade2 python-svn python-dbus meld
        sudo apt-get install -y python-meld3
        sudo apt-get install -y git-core
        sudo apt-get install -y gitk
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum install -y git-core
        sudo yum install -y gitk
    else
        echo "You are about to install on a non supported linux distribution."
    fi        
}


## Install git self tool
Install_tools()
{
    INS_PATH=/usr/libexec/git-core/
    
    if [ -d $INS_PATH ] ; then
        sudo ln -sf $ZZEMACS_ROOT/libexec/git-ediff   $INS_PATH
        sudo ln -sf $ZZEMACS_ROOT/libexec/git-flow/*  $INS_PATH
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


echo -n "Do you need install git self tool? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        try_command Install_tools
        ;;
esac

echo "git diff tools setup end ..."
