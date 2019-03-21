#!/bin/bash
#set -x

## check os
if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

PROXY_CONF_PATH=/etc/apt/apt.conf.d/proxy.conf
rm -f $PROXY_CONF_PATH

echo "remove apt proxy done"
echo "please run 'apt update'"
