#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
ZZEMACS_HOME=$(cd $SCRIPT_ROOT/../.. && pwd)

. $ZZEMACS_HOME/bin/sample.sh

echo "install clang-complete start ..."

case "$OS_DISTRO" in
    "Ubuntu" | "LinuxMint" )
        sudo apt install -y llvm-dev libclang-dev
        ;;
    "Arch" | "Manjaro" )
        sudo pacman -S clang llvm
        ;;
    * )
        echo "You are about to install on a non supported linux distribution."
        ;;
esac

make
sudo mv clang-complete /usr/bin/

echo "install clang-complete end ..."
