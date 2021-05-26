#!/bin/sh

SCRIPT_ROOT=`pwd`

##Import vars and functions
. $SCRIPT_ROOT/sample.sh

echo "install linux-tool begin..."

##package for suse
install_package_suse() {
    sudo zypper install dos2unix
}

##package for ubuntu
install_package_ubuntu() {
    sudo apt install -y curl wget w3m
    sudo apt install -y rofi
    sudo apt install -y rxvt-unicode
    sudo apt install -y openssh-server
    sudo apt install -y tree
    sudo apt install -y tmux vim gvim-gtk3
    sudo apt install -y sbcl
    sudo apt install -y wireless-tools
    sudo apt install -y connect-proxy
    sudo apt install -y cups-pdf
    sudo apt install -y git-email gitk gitg
    sudo apt install -y meld
    sudo apt install -y evolution-ews
    sudo apt install -y dos2unix
    sudo apt install -y xchm
    sudo apt install -y djview4
    sudo apt install -y baobab
    sudo apt install -y pidgin-sipe
    sudo apt install -y sakura
    sudo apt install -y xautolock
    sudo apt install -y synaptic
    sudo apt install -y xdotool     ## xdotool click 4
    sudo apt install -y keynav      ## https://www.semicomplete.com/projects/keynav/
    sudo apt install -y xautomation ## xte "mouseclick 4"
    sudo apt install -y planner
    sudo apt install -y dconf-cli
    sudo apt install -y keynav
    sudo apt install -y trash-cli htop neofetch ncdu
    sudo apt install -y zathura     ## pdf viewer like vim
    sudo apt install -y zathura-cb zathura-djvu zathura-pdf-poppler zathura-ps 
}

##package for centos
install_package_centos() {
    sudo yum install -y dos2unix
    sudo yum install -y net-tools
    sudo yum install -y pciutils        ## lspci
    sudo yum install -y vim
    sudo yum install -y nano
    sudo yum install -y tmux
    sudo yum install -y git gitk
    sudo yum install -y wireless-tools
    sudo yum install -y python3 python3-pip python2-pip
    sudo yum install -y rxvt-unicode sakura
    sudo yum install -y alsa-tools alsa-utils
    sudo yum isntall -y connect-proxy
    sudo yum install -y rfkill
    sudo yum install -y usbutils        ## lsusb
    sudo yum install -y util-linux-ng   ## lsblk
    sudo yum install -y lsscsi          ## lsscsi
    sudo yum install -y bash-completion bash-completion-extras
}

##package for fedora
install_package_fedora() {
    sudo dnf install -y dos2unix
    sudo dnf install -y openssh-server
    sudo dnf install -y tmux vim
    sudo dnf install -y sbcl
    sudo dnf install -y rofi
    sudo dnf install -y rxvt-unicode
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
