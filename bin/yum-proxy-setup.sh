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
    * )
        echo "Usage: `basename $0` host port"
        echo "Usage: no prefix-http/ftp"
        exit 1
        ;;
esac

sed -i '/proxy=/d' /etc/yum.conf
echo "proxy=http://$host:$http_port/" >> /etc/yum.conf

echo "setup yum proxy done"
echo "please re-login"
