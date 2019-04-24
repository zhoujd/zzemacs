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
        "Ubuntu" | "LinuxMint" )
            sudo apt install -y cscope
            sudo apt install -y texinfo
            sudo apt install -y markdown
            sudo apt install -y w3m
            sudo apt install -y silversearcher-ag
            sudo apt install -y socat
            ;;
        "CentOS" )
            sudo yum install -y cscope
            sudo yum install -y texinfo
            ;;
        "Fedora" )
            sudo dnf install -y cscope
            sudo dnf install -y texinfo
            ;;
        "FreeBSD" )
            sudo pkg_add -r w3m
            sudo pkg_add -r cscope
            ;;
        "Arch" | "Manjaro" )
            sudo pacman -S tk cscope w3m
            sudo pacman -S socat
            ;;
        * )
            echo "You are about to install on a non supported linux distribution."
            ;;
    esac
}

echo "Install package for emacs on $OS_DISTRO"
install_package
