#!/bin/sh

if [ "$#" = "1" ] ; then
    netstat -pan | grep $1
else
    echo "$0 <port>"
fi

