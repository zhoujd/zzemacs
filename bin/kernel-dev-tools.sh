#!/bin/sh

SCRIPT_ROOT=`pwd`

##Import vars and functions
. $SCRIPT_ROOT/sample.sh

echo "Install kernel-dev-tool begin..."

##package for suse
install_package_suse() {
    sudo zypper install -y git diffstat
}

##package for ubuntu
install_package_ubuntu() {
    sudo apt install -y flex bison
    sudo apt install -y libelf-dev libssl-dev
}

##package for centos
install_package_centos() {
    sudo yum install -y git diffstat
}

##package for fedora
install_package_fedora() {
    sudo dnf install -y git diffstat
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
