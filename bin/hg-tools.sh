#!/bin/sh

##Import vars and functions
. sample.sh

echo "hg setup start ..."

###Mercurial Books
##http://mercurial.selenic.com/
##http://hginit.com/ (Hg Init: a Mercurial tutorial)
##http://hgbook.red-bean.com/ (Mercurial: The Definitive Guide)

Install_package()
{
    # dectect OS version
    if [ "$LINUX_DISTRO" = "SuSE" ]; then
        echo "Install on suse"
    elif [ "$LINUX_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y mercurial
    elif [ "$LINUX_DISTRO" = "CentOS" ]; then
        sudo yum install -y mercurial
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}

##setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        try_command Install_package
        ;;
esac
