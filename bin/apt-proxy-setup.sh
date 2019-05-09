#!/bin/bash
#set -x

## check root user
if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

## check arguments
case $# in
    2 )
        host=$1
        http_port=$2
        ;;
    3 )
        host=$1
        http_port=$2
        ;;
    * )
        echo "Usage: `basename $0` host port"
        echo "Usage: no prefix-http/ftp"
        exit 1
        ;;
esac

TARGET_DIR=/etc/apt/apt.conf.d
mkdir -p $TARGET_DIR
cat <<EOF > $TARGET_DIR/zz-proxy.conf
## This is for apt proxy
Acquire::http::Proxy "http://$host:$http_port/";
Acquire::https::Proxy "http://$host:$http_port/";
Acquire::ftp::Proxy "http://$host:$http_port/";
EOF

echo "setup apt proxy done"
echo "please run 'apt update'"
