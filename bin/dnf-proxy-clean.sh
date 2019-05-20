#!/bin/bash
#set -x

## check root user
if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

sed -i '/proxy=/d' /etc/dnf/dnf.conf
sed -i '/fastestmirror=/d' /etc/dnf/dnf.conf

echo "clean dnf proxy done"
echo "please run 'dnf update'"
