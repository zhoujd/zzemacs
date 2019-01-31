#!/bin/bash

if [ $# != 2 ] ; then
    echo "Usage: `basename $0` host port"
    exit 1
fi

proxy_url=http://$1:$2/

git config --global http.proxy  $proxy_url
git config --global https.proxy $proxy_url

git config --global --list | grep -E https?.proxy
