#!/bin/sh

CURRENT_ROOT=`pwd`

echo for python develop start ...

cd $CURRENT_ROOT/pymacs/
sudo python pppp -C ppppconfig.py pppp.rst.in pymacs.el.in pymacs.rst.in Pymacs.py.in contrib tests
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/rope/
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/ropemode/
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/ropemacs/
sudo python setup.py install
cd $CURRENT_ROOT

sudo cp -f $CURRENT_ROOT/pycomplete.py /usr/local/lib/python2.7/dist-packages/
rm -f $CURRENT_ROOT/../../site-lisp/pymacs.elc

echo for python develop end   ...
