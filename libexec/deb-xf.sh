#!/bin/sh

if [ $# != 1 ]; then
    echo "Usage: `basename $0` <1.deb>"
    exit 1
fi

ar vx $1
tar xf data.tar.xz
