#!/bin/bash

yum install -y glibc-static

wget http://ftp.gnu.org/gnu/gcc/gcc-4.7.2/gcc-4.7.2.tar.bz2

tar xjvf gcc-4.7.2.tar.bz2
cd gcc-4.7.2
./contrib/download_prerequisites

mkdir build
cd build
../configure --prefix=/usr --libdir=/usr/lib64 --enable-languages=c,c++ --disable-multilib --disable-checking

make -j12
make install
