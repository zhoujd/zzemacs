#!/bin/sh

##Import vars and functions
. sample.sh

echo "hg setup start ..."

###Mercurial Books
##http://mercurial.selenic.com/
##http://hginit.com/ (Hg Init: a Mercurial tutorial)
##http://hgbook.red-bean.com/ (Mercurial: The Definitive Guide)

install_package() {
    case "$OS_DISTRO" in
        "SUSE" )
            sudo zypper install -y mercurial
            sudo zypper install -y python-devel
            sudo zypper install -y python-docutils
            ;;
        "Ubuntu" )
            sudo apt-get install -y mercurial
            sudo apt-get install -y python-devel
            sudo apt-get install -y python-docutils
            ;;
        "CentOS" )
            sudo yum install -y mercurial
            sudo yum install -y python-devel
            sudo yum install -y python-docutils
            ;;
        "Fedora" )
            sudo dnf install -y mercurial
            sudo dnf install -y python-devel
            sudo dnf install -y python-docutils
            ;;
        "Arch" | "Manjaro" )
            sudo pacman -S mercurial
        * )
            echo "You are about to install on a non supported linux distribution."
            ;;
    esac
}

##setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        run_cmd install_package
        ;;
esac

echo "hg setup end ..."
