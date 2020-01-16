#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

##http://www.python.org/
##http://www.ipython.org/
##http://archive.ipython.org/release/
##https://pypi.python.org/pypi/pyreadline

PIP_PAR="--timeout 60"

echo "For python develop start ..."

py2_deps() {
    pip install $PIP_PAR -r $SCRIPT_ROOT/py2.txt
}

py3_deps() {
    sudo apt install -y python3-venv
    pip3 install $PIP_PAR -r $SCRIPT_ROOT/py3.txt
}

case $1 in
    py3 )
        py3_deps
        ;;
    * )
        py2_deps
        ;;
esac

echo "For python develop end ..."
