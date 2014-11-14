#!/bin/sh

CURRENT_ROOT=`pwd`

echo "for perl develop start ..."

echo "==>1 install EPL"
cd $CURRENT_ROOT/pymacs
perl Makefile.PL
make
sudo make install
cd $CURRENT_ROOT

echo "for perl develop end ..."
