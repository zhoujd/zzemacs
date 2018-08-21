#!/bin/bash

## /etc/yum.conf
# proxy=http://username:password@yourproxy:8080

## /etc/apt/apt.conf
# Acquire::http::proxy "http://127.0.0.1:8000";  
# Acquire::ftp::proxy "ftp://127.0.0.1:8000";  
# Acquire::https::proxy "https://127.0.0.1:8000";

if [ $# != 1 ] ; then
    echo "Usage: `basename $0` [host:port]"
    echo "Usage: no prefix-http/ftp"
    exit 1
fi

echo "Setup proxy start ..."

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    PROXY_SCRIPT=$HOME/.bashrc.d/99_proxy.sh
else
    SETUP_ROOT=$(cd $(dirname $0) && pwd)
    SETUP_SYS=1
    if [ $SETUP_SYS -ne 0 ]; then
        if [ $EUID -ne 0 ]; then
            echo "You must be a root user" 2>&1
            exit 1
        fi
        PROXY_SCRIPT=/etc/profile.d/proxy.sh
    else
        PROXY_SCRIPT=$HOME/.bashrc
    fi
fi

cat <<EOF >> $PROXY_SCRIPT
## This is for bash proxy
export http_proxy=http://$1/
export https_proxy=\$http_proxy
export ftp_proxy=\$http_proxy
export no_proxy="localhost,127.0.0.0/8,::1,10.0.0.0/8,192.168.0.0/16"

EOF

echo "Setup proxy end ..."
