#!/bin/bash

source /etc/os-release

install_cc() {
    case $ID in
        ubuntu|debian )
            echo "Run on $ID"
            sudo apt install -y ccls clangd
            ;;
        centos )
            echo "Run on $ID"
            sudo yum install -y epel-release
            sudo yum install -y ccls clangd
            ;;
        * )
            echo "Unsupport OS: $ID"
            ;;
    esac
    echo "Install cc tool done"
}


install_py() {
    PIP_ARGS=(
        --timeout 60
        -i https://pypi.tuna.tsinghua.edu.cn/simple
    )
    pip3 install ${PIP_ARGS[@]} python-lsp-server[all]
    echo "Install python tool done"
}

case $1 in
    cc )
        install_cc
        ;;
    py )
        install_py
        ;;
    -h|--help )
        echo "$(basename $0) {cc|py|-h|--help}"
        ;;
    all|* )
        install_cc
        install_py
        ;;
esac
