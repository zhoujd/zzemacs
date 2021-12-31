#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)

. $ZZEMACS_ROOT/bin/sample.sh

# dectect OS version
case "$OS_DISTRO" in
    "Ubuntu" | "LinuxMint" )
        GLIBC_FOLDER=/usr/lib/x86_64-linux-gnu
        GLIBC_PATH=$GLIBC_FOLDER/libc.so.6
        ;;
    "CentOS" )
        GLIBC_FOLDER=/lib64
        GLIBC_PATH=$GLIBC_FOLDER/libc.so.6
        ;;
    * )
        echo "Non supported linux distribution."
        exit 1
        ;;
esac

## List GLIBC files
ls -l  $GLIBC_FOLDER/libc.so.*

## List GLIBC  version
echo "Check $GLIBC_PATH ..."
strings $GLIBC_PATH | grep GLIBC
