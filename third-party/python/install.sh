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

VENV_ROOT=$HOME/.venv
VENV_PATH=$VENV_ROOT/emacs
PIP_BIN=$VENV_PATH/bin/pip3
PIP_PAR="--timeout 60"

echo "For python develop start ..."

py3_deps() {
    $PIP_BIN install $PIP_PAR -r $SCRIPT_ROOT/requirements.txt
}

setup_flake8() {
    echo "Setup flake8 configure"
    ln -sfvT $SCRIPT_ROOT/flake8 ~/.config/flake8
}

case $1 in
    py3 )
        py3_deps
        ;;
    flake8 )
        setup_flake8
        ;;
    * )
        echo "Usage: $0 {py3|flake8}"
        ;;
esac

echo "For python develop end ..."
