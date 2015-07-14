#!/bin/sh

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    echo "This script is not support on windows."
    exit 0
fi

GLIBC_FOLDER=/lib64
GLIBC_PATH=$GLIBC_FOLDER/libc.so.6

## List GLIBC files
ls -l  $GLIBC_FOLDER/libc*

## List GLIBC  version
echo "Check $GLIBC_PATH ..."
strings $GLIBC_PATH | grep GLIBC
