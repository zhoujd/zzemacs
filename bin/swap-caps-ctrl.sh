#!/bin/sh

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

##Get Script path
SETUP_ROOT=$(cd $(dirname $0) && pwd)

##Import vars and functions
. $SETUP_ROOT/sample.sh

echo "Swap caps and left ctrl start ..."

ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)
xmodmap $ZZEMACS_ROOT/etc/swap-caps-ctrl.xmodmap 2>/dev/null

echo "Swap caps and left ctrl end ..."
