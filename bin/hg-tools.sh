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
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper install -y python-devel
        sudo zypper install -y python-docutils
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y mercurial
        sudo apt-get install -y python-docutils
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum install -y mercurial
        sudo yum install -y python-devel
        sudo yum install -y python-docutils
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}

##setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        run_cmd Install_package
        ;;
esac
