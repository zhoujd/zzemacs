#!/bin/sh

if [ $# != 1 ]; then
    echo "Usage: $0 <1.rpm>"
    exit 1
fi

ar vx $1
tar xf data.tar.xz
