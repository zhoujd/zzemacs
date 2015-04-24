#!/bin/bash

HG_INS_HOME=$(cd $(dirname $0) && pwd)

## Source sample.sh
. $HG_INS_HOME/sample.sh

## Install package for git
Install_package()
{
    # dectect OS version
    if [ "$OS_DISTRO" = "SUSE" ]; then
        sudo zypper install -y python-docutils
        sudo zypper install -y python-devel
    elif [ "$OS_DISTRO" = "Ubuntu" ]; then
        sudo apt-get install -y python-docutils
        sudo apt-get install -y python-dev
    elif [ "$OS_DISTRO" = "CentOS" ]; then
        sudo yum install -y  python-docutils
        sudo yum install -y  python-devel
    else
        echo "You are about to install on a non supported linux distribution."
    fi        
}

Install_src_hg()
{
    ##Init install temp directory
    INS_TEMP_DIR=$HOME/Downloads/hg-src-install
    rm -rf $INS_TEMP_DIR
    mkdir -p $INS_TEMP_DIR
    
    pushd $INS_TEMP_DIR
    
    wget http://selenic.com/hg/archive/tip.tar.gz
    tar xf tip.tar.gz
    mv Mercurial-* Mercurail-latest
    cd Mercurail-latest
    make all
    sudo make PREFIX=/usr install
    
    popd
}

main()
{
    ## setup packages
    echo -n "Do you need install packages? (y/N): "
    read answer
    case "$answer" in
        "Y" | "y" )
            try_command Install_package
            ;;
    esac

    ## setup hg from source
    try_command Install_src_hg
   
    echo "Install hg from source finished"
}

main
