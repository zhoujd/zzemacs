#!/bin/sh

TARGET=connect
INS_PATH=/usr/bin

echo "Build connect.c to $TARGET ..."
gcc connect.c -o $TARGET

echo "Install $TARGET to $INS_PATH ..."
sudo mv $TARGET $INS_PATH
