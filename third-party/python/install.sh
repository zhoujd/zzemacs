#!/bin/sh

echo for python develop start ...

cd ./pymacs/
sudo python pppp -C ppppconfig.py pppp.rst.in pymacs.el.in pymacs.rst.in Pymacs.py.in contrib tests
sudo python setup.py install
cd ..

cd ./rope/
sudo python setup.py install
cd ..

cd ./ropemode/
sudo python setup.py install
cd ..

cd ./ropemacs/
sudo python setup.py install
cd ..

sudo cp ./pycomplete.py /usr/local/lib/python2.7/

echo for python develop end   ...
