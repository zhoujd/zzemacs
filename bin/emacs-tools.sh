#!/bin/sh

EMACS_TOOLS_ROOT=`pwd`

##Import vars and functions
. $EMACS_TOOLS_ROOT/sample.sh


install_package() {
    case "$OS_DISTRO" in
        "SUSE" )
            sudo zypper install -y cscope
            sudo zypper install -y texinfo
            ;;
        "Ubuntu" )
            sudo apt-get install -y cscope
            sudo apt-get install -y texinfo
            sudo apt-get install -y markdown
            sudo apt-get install -y w3m
            sudo apt-get install -y silversearcher-ag
            ;;
        "CentOS" )
            sudo yum install -y cscope
            sudo yum install -y texinfo
            ;;
        "Fedora" )
            sudo dnf install -y cscope
            sudo dnf install -y texinfo
            ## If want to install emacs with dnf tool
            #sudo dnf install -y emacs emacs-auto-complete emacs-auto-complete-el
            ;;
        "FreeBSD" )
            sudo pkg_add -r w3m
            sudo pkg_add -r cscope
            ;;
        "Arch" | "Manjaro" )
            sudo pacman -S tk cscope w3m
            ;;
        t )
            echo "You are about to install on a non supported linux distribution."
            ;;
    esac
}

echo "Install package for emacs on $OS_DISTRO"
install_package
