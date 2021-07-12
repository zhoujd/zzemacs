#!/bin/sh

## http://www.emacswiki.org/emacs/MovingTheCtrlKey

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

swapcaps() {
    setxkbmap -option ctrl:swapcaps     # Swap Left Control and Caps Lock
}

nocaps() {
    setxkbmap -option ctrl:nocaps       # Make Caps Lock a Control key
}

swapcaps

echo "Swap caps and left ctrl end ..."
