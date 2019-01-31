#!/bin/bash
#set -x

if [ $# != 2 ] ; then
    echo "Usage: `basename $0` host port"
    echo "Usage: no prefix-http/ftp"
    exit 1
fi

echo "Setup proxy start ..."

host=$1
http_port=$2
socks_port=1080

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
## This is for proxy configure
export http_proxy=http://$host:$http_port/
export https_proxy=\$http_proxy
export ftp_proxy=\$http_proxy
export no_proxy=localhost,127.0.0.0/8,::1,10.0.0.0/8,192.168.0.0/16
export socks_host=$host
export socks_port=$socks_port
EOF

echo "Setup proxy end ..."
