#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

##http://www.python.org/
##http://www.ipython.org/
##http://archive.ipython.org/release/
##https://pypi.python.org/pypi/pyreadline

#pip install pip -U
#pip install -i https://pypi.tuna.tsinghua.edu.cn/simple pip -U
#pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
#pip config set global.index-url https://mirrors.aliyun.com/pypi/simple/

PIP_PAR="--user --timeout 60"

echo "For python develop start ..."

py3_deps() {
    pip3 install pip -U
    pip3 install $PIP_PAR -r $SCRIPT_ROOT/py3.txt
}

setup_flake8() {
    echo "Setup flake8 configure"
    ln -sfvT $SCRIPT_ROOT/flake8 ~/.config/flake8
}

case $1 in
    py3 )
        py3_deps
        setup_flake8
        ;;
    flake8 )
        setup_flake8
        ;;
    * )
        echo "Usage: $0 {py3|flake8}"
        ;;
esac

echo "For python develop end ..."
