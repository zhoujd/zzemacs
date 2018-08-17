#!/bin/sh

LINUX_TOOL_ROOT=`pwd`

##Import vars and functions
. $LINUX_TOOL_ROOT/sample.sh

echo "install linux-tool begin..."

##package for suse
install_package_suse() {
    sudo zypperl install dos2unix
}

##package for ubuntu
install_package_ubuntu() {
    sudo apt-get install -y texlive
    sudo apt-get install -y texinfo
    sudo apt-get install -y xmlto

    sudo apt-get install -y openssh-server
    sudo apt-get install -y tree

    ##http://www.gentoo-wiki.info/SSH_Reverse_Tunnel
    ##http://www.cnblogs.com/eshizhan/archive/2012/07/16/2592902.html
    ##ssh -N -v -D localhost:8527 root@remote_ssh_server -p remote_ssh_port
    ##autossh -M 5122 -N -v -D localhost:8527 root@remote_ssh_server -p remote_ssh_port
    sudo apt-get install -y autossh

    ##http://cntlm.sourceforge.net
    #sudo apt-get install -y cntlm
    
    ##easy split windows for multi-term
    #sudo apt-get install -y terminator

    ##gnome
    #sudo apt-get install -y gnome-panel gnome-settings-daemon metacity nautilus gnome-terminal

    ##xfce4
    #sudo apt-get install -y xfce4
    #sudo apt-get install -y lightdm lightdm-gtk-greeter
}

##package for centos
install_package_centos() {
    sudo yum install dos2unix
}

##package for fedora
install_package_centos() {
    sudo dnf install dos2unix
}

# dectect OS version
if [ "$OS_DISTRO" = "SUSE" ]; then
    run_cmd install_package_suse
elif [ "$OS_DISTRO" = "Ubuntu" ]; then
    run_cmd install_package_ubuntu
elif [ "$OS_DISTRO" = "CentOS" ]; then
    run_cmd install_package_centos
elif [ "$OS_DISTRO" = "Fedora" ]; then
    run_cmd install_package_fedora
else
    echo "You are about to install on a non supported linux distribution."
fi

echo "install linux-tool end..."
