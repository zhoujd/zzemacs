#!/bin/sh

##http://www.python.org/
##http://www.ipython.org/
##http://archive.ipython.org/release/
##https://pypi.python.org/pypi/pyreadline

## remove install files
# cat setup.log｜xagrs rm -rf

CURRENT_ROOT=`pwd`

echo "for python develop start ..."

# virtualenv
pip install virtualenv epc

# Either of these
pip install rope
pip install jedi
# flake8 for code checks
pip install flake8
# importmagic for automatic imports
pip install importmagic
# and autopep8 for automatic PEP8 formatting
pip install autopep8
# and yapf for code formatting
pip install yapf


echo "for python develop end ..."
