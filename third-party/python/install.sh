#!/bin/sh

##http://www.python.org/
##http://www.ipython.org/
##http://archive.ipython.org/release/
##https://pypi.python.org/pypi/pyreadline

CURRENT_ROOT=`pwd`

echo "for python develop start ..."

py2_deps() {
    # virtualenv
    pip install virtualenv
    pip install epc
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
}

py3_deps() {
    # virtualenv
    pip3 install virtualenv
    pip3 install epc
    # Either of these
    pip3 install rope
    pip3 install jedi
    # flake8 for code checks
    pip3 install flake8
    # importmagic for automatic imports
    pip3 install importmagic
    # and autopep8 for automatic PEP8 formatting
    pip3 install autopep8
    # and yapf for code formatting
    pip3 install yapf
}

case $1 in
    py3 )
        py3_deps
        ;;
    * )
        py2_deps
        ;;
esac

echo "for python develop end ..."
