#!/bin/sh

##http://www.python.org/
##http://www.ipython.org/
##http://archive.ipython.org/release/
##https://pypi.python.org/pypi/pyreadline

CURRENT_ROOT=`pwd`

echo for python develop start ...

cd $CURRENT_ROOT/pymacs
sudo python pppp -C ppppconfig.py pppp.rst.in pymacs.el.in pymacs.rst.in Pymacs.py.in contrib tests
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/rope
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/ropemode
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/ropemacs
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/pyreadline
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/pydb
sudo python setup.py install
cd $CURRENT_ROOT

cd $CURRENT_ROOT/zzpytools
sudo python setup.py install
cd $CURRENT_ROOT

echo for python develop end   ...
