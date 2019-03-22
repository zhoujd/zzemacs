#!/bin/bash

if [ $# != 2 ] ; then
    echo "Usage: `basename $0` host port"
    exit 1
fi

if [ "$OS" = "Windows_NT" ] ; then
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd -W)
else
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
fi

PROXY_URL=http://$1:$2/

git config --global http.proxy  $PROXY_URL
git config --global core.gitproxy $SCRIPT_ROOT/git-proxy-wrapper.sh

git config --global --list | grep -E http.proxy
git config --global --list | grep -E core.gitproxy

echo "git proxy setup done"
