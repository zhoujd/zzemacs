#!/bin/bash

if [ $# != 2 ] ; then
    echo "Usage: `basename $0` host port"
    exit 1
fi

git config --global http.proxy http://$1:$2
git config --global https.proxy https://$1:$2
