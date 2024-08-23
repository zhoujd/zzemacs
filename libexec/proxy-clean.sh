#!/bin/bash
#set -x

## check os
case "$OS" in
    "Windows_NT" )
        echo "clean proxy on Windows"
        rm -f $HOME/.bashrc.d/99-proxy.sh
        ;;
    * )
        echo "clean proxy on Linux"
        if [ $EUID -ne 0 ]; then
            echo "You must be a root user" 2>&1
            exit 1
        fi

        unset http_proxy HTTP_PROXY
        unset https_proxy HTTPS_PROXY
        unset ftp_proxy FTP_PROXY
        unset socks_proxy SOCKS_PROXY
        unset all_proxy ALL_PROXY
        unset no_proxy  NO_PROXY

        rm -f /etc/profile.d/zz-proxy.sh
        ;;
esac

echo "remove proxy done."
echo "please reboot"
