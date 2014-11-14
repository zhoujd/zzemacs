#!/bin/sh

TARGET=connect
INS_PATH=/usr/bin

echo "Build connect.c ..."
gcc connect.c -o $TARGET

echo "Install connect to  ..."
sudo mv $TARGET $INS_PATH
