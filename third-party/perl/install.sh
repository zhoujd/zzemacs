#!/bin/sh

CURRENT_ROOT=`pwd`

echo "for perl develop start ..."

echo "==>1 install EPL"
cd $CURRENT_ROOT/EPL
perl Makefile.PL
make
sudo make install
cd $CURRENT_ROOT

echo "==>2 install Digest-MD5"
cd $CURRENT_ROOT/Digest-MD5
perl Makefile.PL
make
sudo make install
cd $CURRENT_ROOT

echo "for perl develop end ..."
