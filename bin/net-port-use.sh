#!/bin/sh

# lsos -i
# lsof -i:8556
# killall <program-name>

if [ "$#" = "1" ] ; then
    netstat -pan | grep $1
else
    echo "$0 <port>"
fi

