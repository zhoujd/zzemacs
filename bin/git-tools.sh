#!/bin/sh

## Git script path
GIT_TOOLS_ROOT=$(cd $(dirname $0) && pwd)
ZZEMACS_ROOT=$(cd $GIT_TOOLS_ROOT/.. && pwd)

##Import vars and functions
. $GIT_TOOLS_ROOT/sample.sh

## Install package for git
install_package() {
    # dectect OS version
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper install -y texinfo
        sudo zypper install -y git-core
        sudo zypper install -y gitk
        sudo zypper install -y git-email
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y texinfo
        sudo apt-get install -y git-core
        sudo apt-get install -y gitk
        sudo apt-get install -y git-email
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum install -y texinfo
        sudo yum install -y git-core
        sudo yum install -y gitk
        sudo yum install -y git-email
    elif [ "$OS_DISTRO" = "Fedora" ]; then
        sudo dnf install -y texinfo
        sudo dnf install -y git-core
        sudo dnf install -y gitk
        sudo yum install -y git-email
    else
        echo "You are about to install on a non supported linux distribution."
    fi        
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
