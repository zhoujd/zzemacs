#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
echo "For python os develop start ..."


case $1 in
    centos )
        sudo yum install -y python3-pip
        ;;
    ubuntu )
        sudo apt install -y python3-pip
        sudo apt install -y python3-venv
        sudo apt install -y python3-virtualenv
        ;;
    * )
        echo "Usage: $0 {centos|ubuntu}"
        ;;
esac

echo "For python os develop end ..."
