#!/bin/bash
#set -x

## check os
case "$OS" in
    "Windows_NT" )
        echo "Setup proxy on Windows"
        rm -f $HOME/.bashrc.d/99-proxy.sh
        ;;
    * )
        echo "Setup proxy on Linux"
        if [ $EUID -ne 0 ]; then
            echo "You must be a root user" 2>&1
            exit 1
        fi

        rm -f /etc/profile.d/zz-proxy.sh
        ;;
esac

echo "remove proxy done."
echo "please reboot"
