#!/bin/sh

EMACS_TOOLS_ROOT=`pwd`

##Import vars and functions
. $EMACS_TOOLS_ROOT/sample.sh


install_package() {
    # dectect OS version
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper install -y cscope
        sudo zypper install -y texinfo
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y gmrun
        sudo apt-get install -y cscope
        sudo apt-get install -y texinfo
        sudo apt-get install -y markdown
        sudo apt-get install -y w3m
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum install -y cscope
        sudo yum install -y texinfo
    elif [ "$OS_DISTRO" = "Fedora" ]; then
        sudo dnf install -y cscope
        sudo dnf install -y texinfo
        ## If want to install emacs with dnf tool
        #sudo dnf install -y emacs emacs-auto-complete emacs-auto-complete-el
    elif [ "$OS_DISTRO" = "FreeBSD" ]; then
        sudo pkg_add -r w3m
        sudo pkg_add -r cscope
        sudo pkg_add -r gmrun
    elif [ "$OS_DISTRO" = "Arch" ]; then
        sudo pacman -S tk cscope w3m
    else
        echo "You are about to install on a non supported linux distribution."
    fi
}

echo "Install package for emacs on $OS_DISTRO"
install_package
