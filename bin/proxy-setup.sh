#!/bin/bash
#set -x

## check arguments
case $# in
    2 )
        host=$1
        http_port=$2
        socks_port=1080
        ;;
    3 )
        host=$1
        http_port=$2
        socks_port=$3
        ;;
    * )
        echo "Usage: `basename $0` host port [socks_port]"
        echo "Usage: no prefix-http/ftp"
        exit 1
        ;;
esac

## check os
case "$OS" in
    "Windows_NT" )
        echo "Setup proxy on Windows"
        
        PROXY_SCRIPT=$HOME/.bashrc.d/99-proxy.sh
        ;;
    * )
        echo "Setup proxy on Linux"
        
        if [ $EUID -ne 0 ]; then
            echo "You must be a root user" 2>&1
            exit 1
        fi

        PROXY_SCRIPT=/etc/profile.d/zz-proxy.sh
        ;;
esac

cat <<EOF > $PROXY_SCRIPT
## This is for proxy configure
export http_proxy=http://$host:$http_port/
export https_proxy=\$http_proxy
export ftp_proxy=\$http_proxy
export no_proxy=localhost,127.0.0.0/8,::1,10.0.0.0/8,192.168.0.0/16
export socks_host=$host
export socks_port=$socks_port
EOF
