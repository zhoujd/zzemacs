#!/bin/sh

SCRIPT_ROOT=`pwd`

##Import vars and functions
. $SCRIPT_ROOT/sample.sh

echo "install linux-tool begin..."

##package for suse
install_package_suse() {
    sudo zypperl install dos2unix
}

##package for ubuntu
install_package_ubuntu() {
    sudo apt install -y rofi
    sudo apt install -y rxvt-unicode
    sudo apt install -y openssh-server
    sudo apt install -y tree
    sudo apt install -y tmux byobu
    sudo apt install -y wireless-tools
    sudo apt install -y connect-proxy
}

##package for centos
install_package_centos() {
    sudo yum install dos2unix
}

##package for fedora
install_package_fedora() {
    sudo dnf install dos2unix
}

# dectect OS version
case "$OS_DISTRO" in 
    "SUSE" )
        run_cmd install_package_suse
        ;;
    "Ubuntu" | "LinuxMint" )
        run_cmd install_package_ubuntu
        ;;
    "CentOS" )
        run_cmd install_package_centos
        ;;
    "Fedora" )
        run_cmd install_package_fedora
        ;;
    * )
        echo "You are about to install on a non supported linux distribution."
esac

echo "install linux-tool end..."
