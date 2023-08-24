#!/bin/sh

if [ $# != 1 ]; then
    echo "Usage: `basename $0` <1.rpm>"
    exit 1
fi

rpm2cpio $1 | cpio -ivd

