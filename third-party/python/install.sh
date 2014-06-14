#!/bin/sh

##http://www.python.org/
##http://www.ipython.org/
##http://archive.ipython.org/release/
##https://pypi.python.org/pypi/pyreadline

CURRENT_ROOT=`pwd`

echo "for python develop start ..."

echo "==>1 install pymacs"
cd $CURRENT_ROOT/pymacs
python pppp -C ppppconfig.py pppp.rst.in pymacs.el.in pymacs.rst.in Pymacs.py.in contrib tests
python setup.py install
cd $CURRENT_ROOT

echo "==>2 install rope"
cd $CURRENT_ROOT/rope
python setup.py install
cd $CURRENT_ROOT

echo "==>3 install ropemode"
cd $CURRENT_ROOT/ropemode
python setup.py install
cd $CURRENT_ROOT

echo "==>4 install ropemacs"
cd $CURRENT_ROOT/ropemacs
python setup.py install
cd $CURRENT_ROOT

echo "==>5 install pyreadline"
cd $CURRENT_ROOT/pyreadline
python setup.py install
cd $CURRENT_ROOT

echo "==>6 install pydb"
cd $CURRENT_ROOT/pydb
./configure
make
make install
cd $CURRENT_ROOT

echo "==>7 install pycomplete"
cd $CURRENT_ROOT/pycomplete
python setup.py install
cd $CURRENT_ROOT

echo "for python develop end ..."
