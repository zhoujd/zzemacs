#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

##http://www.python.org/
##http://www.ipython.org/
##http://archive.ipython.org/release/
##https://pypi.python.org/pypi/pyreadline

#pip install pip -U
#pip install -i https://pypi.tuna.tsinghua.edu.cn/simple pip -U
#pip config set global.index-url https://pypi.tuna.tsinghua.edu.cn/simple
#pip config set global.index-url https://mirrors.aliyun.com/pypi/simple
#pip config set global.index-url "https://pypi.tuna.tsinghua.edu.cn/simple https://mirrors.aliyun.com/pypi/simple"

VENV_ROOT=$HOME/.venv
VENV_PATH=$VENV_ROOT/emacs
PIP_BIN=$VENV_PATH/bin/pip3
PIP_ARGS=(
    --timeout 60
    -i https://pypi.tuna.tsinghua.edu.cn/simple pip
)

echo "For python develop start ..."

py3_deps() {
    $PIP_BIN install ${PIP_ARGS[@]} -r $SCRIPT_ROOT/requirements.txt
}

setup_flake8() {
    echo "Setup flake8 configure"
    local target=$HOME/.config
    mkdir -p $target
    ln -sfvT $SCRIPT_ROOT/flake8 $target/flake8
}

setup_pylsp() {
    echo "Setup pylsp configure"
    local target=$HOME/.local/bin
    mkdir -p $target
    cp -fv $SCRIPT_ROOT/pylsp-wrapper $target
}

usage() {
    local app=$(basename $0)
    cat <<EOF
Usage: $app {py3|flake8|pylsp|all}
EOF
}

case $1 in
    py3 )
        py3_deps
        ;;
    flake8 )
        setup_flake8
        ;;
    pylsp )
        setup_pylsp
        ;;
    all )
        py3_deps
        setup_pylsp
        ;;
    * )
        usage
        ;;
esac

echo "For python develop end ..."
