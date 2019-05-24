#!/bin/sh

## Git script path
GIT_TOOLS_ROOT=$(cd $(dirname $0) && pwd)
ZZEMACS_ROOT=$(cd $GIT_TOOLS_ROOT/.. && pwd)

##Import vars and functions
. $GIT_TOOLS_ROOT/sample.sh

## Install package for git
install_package() {
    case "$OS_DISTRO" in
        "SUSE" )
            sudo zypper install -y git-core
            sudo zypper install -y gitk
            sudo zypper install -y git-email
            ;;
        "Ubuntu" | "LinuxMint" )
            sudo apt-get install -y git-core
            sudo apt-get install -y gitk
            sudo apt-get install -y git-email
            sudo apt-get install -y tig
            ;;
        "CentOS" )
            sudo yum install -y git-core
            sudo yum install -y gitk
            sudo yum install -y git-email
            ;;
        "Fedora" )
            sudo dnf install -y git-core
            sudo dnf install -y gitk
            sudo yum install -y git-email
            ;;
        "Arch" | "Manjaro" )
            sudo pacman -S tk
            ;;
        * )
            echo "You are about to install on a non supported linux distribution."
            ;;
    esac
}


## Install git self tool
install_tools() {
    INS_PATH=$(git --exec-path)
    
    if [ -d $INS_PATH ] ; then
        echo "Install git tools on $INS_PATH"
        sudo ln -sf $ZZEMACS_ROOT/libexec/git-ediff $INS_PATH
    else
        echo "Not git install on system"
    fi
}

## setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        run_cmd install_package
        ;;
esac


echo -n "Do you need install git self tool? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        run_cmd install_tools
        ;;
esac

echo "git diff tools setup end ..."
