#!/bin/bash
#set -x

if [ $# != 2 ] ; then
    echo "Usage: `basename $0` host port"
    echo "Usage: no prefix-http/ftp"
    exit 1
fi

echo "Setup proxy start ..."

## check run os
if [ "$OS" = "Windows_NT" ] ; then
    PROXY_SCRIPT=$HOME/.bashrc.d/99-proxy.sh
else
    if [ $EUID -ne 0 ]; then
        echo "You must be a root user" 2>&1
        exit 1
    fi
    PROXY_SCRIPT=/etc/profile.d/zz-proxy.sh
fi

cat <<EOF > $PROXY_SCRIPT
## This is for bash proxy
export http_proxy=http://$1:$2/
export https_proxy=\$http_proxy
export ftp_proxy=\$http_proxy
export no_proxy="localhost,127.0.0.0/8,::1,10.0.0.0/8,192.168.0.0/16"

export socks_host=$1
export socks_port=1080
EOF

echo "Setup proxy end ..."
