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

sed -i '/proxy=/d' /etc/dnf/dnf.conf
echo "proxy=http://$host:$http_port/" >> /etc/dnf/dnf.conf

sed -i '/fastestmirror=/d' /etc/dnf/dnf.conf
echo "fastestmirror=true" >> /etc/dnf/dnf.conf

echo "setup dnf proxy done"
echo "please run 'dnf update'"
