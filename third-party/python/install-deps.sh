#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
echo "For python os develop start ..."

. /etc/os-release

case $ID in
    centos )
        echo "Install on $ID"
        sudo yum install -y python3-pip
        sudo yum install -y python3-venv
        sudo yum install -y python3-virtualenv
        ;;
    ubuntu | debian )
        echo "Install on $ID"
        sudo apt install -y python3-pip
        sudo apt install -y python3-venv
        sudo apt install -y python3-virtualenv
        ;;
    * )
        echo "Install on $ID is not supported"
        ;;
esac

echo "For python os develop end ..."
