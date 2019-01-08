#!/bin/sh

SCRIPT_ROOT=`pwd`

##Import vars and functions
. $SCRIPT_ROOT/sample.sh

echo "Install linux-dev-tool begin..."

##package for ubuntu
install_package_suse() {
    sudo zypper install -y gcc gcc-c++ automake git gitk diffstat
    sudo zypper install -y ncurses-devel ncurses zlib-devel
    sudo zypper install -y tk tk-devel
    sudo zypper install -y flex cmake bison libtool perl-Archive-Zip
    sudo zypper install -y libpciaccess0-devel xorg-x11-devel libdrm-devel
    sudo zypper install -y libreoffice
}

##package for ubuntu
install_package_ubuntu() {
    sudo apt-get install -y libncurses5-dev libpthread-stubs0-dev libpciaccess-dev libxvmc-dev
    sudo apt-get install -y xorg-dev autoconf libtool yasm
    sudo apt-get install -y x11proto-dri2-dev
    sudo apt-get install -y xutils-dev
    sudo apt-get install -y g++
    sudo apt-get install -y git gitk diffstat
}

##package for centos
install_package_centos() {
    sudo yum install -y git diffstat
    sudo yum install -y gcc cmake gcc-c++ autoconf automake libtool libdrm-devel kernel-headers
    sudo yum install -y libX11-devel xorg-x11-server-devel libpciaccess-devel libXext-devel libXfixes-devel
    sudo yum install -y redhat-lsb-core libpciaccess-devel
    sudo yum install -y bison flex expat-devel
    sudo yum install -y patch rpm-build libudev-devel
    sudo yum install -y libpng12
    sudo yum install -y mesa-dri-drivers #(if system is centos minimal installation, not desktop)
}

##package for fedora
install_package_fedora() {
    sudo dnf install -y git diffstat
    sudo dnf install -y gcc cmake gcc-c++ autoconf automake libtool libdrm-devel kernel-headers
    sudo dnf install -y libX11-devel xorg-x11-server-devel libpciaccess-devel libXext-devel libXfixes-devel
    sudo dnf install -y redhat-lsb-core libpciaccess-devel
    sudo dnf install -y bison flex expat-devel
    sudo dnf install -y patch rpm-build libudev-devel
    sudo dnf install -y libpng12
    sudo dnf install -y mesa-dri-drivers #(if system is centos minimal installation, not desktop)
}

# dectect OS version
case "$OS_DISTRO" in
    "SUSE" )
        run_cmd install_package_suse
        ;;
    "Ubuntu" )
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
        ;;
esac

echo "Install linux-dev-tool end..."
