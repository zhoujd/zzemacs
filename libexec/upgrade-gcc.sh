#!/bin/bash

## yum install -y glibc-static

version=4.7.2

wget http://ftp.gnu.org/gnu/gcc/gcc-${version}/gcc-${version}.tar.bz2

tar xjvf gcc-${version}.tar.bz2
cd gcc-${version}
./contrib/download_prerequisites

mkdir build
cd build
../configure --prefix=/usr --libdir=/usr/lib64 --enable-languages=c,c++ --disable-multilib --disable-checking

make -j12
sudo make install

echo "upgrade to gcc $version done"
