#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$HOME/zzemacs

setup_zzemacs() {
    local install_cmd=$ZZEMACS_ROOT/docker/script/install.sh
    if [ -x $install_cmd ]; then
        echo "Setup zzemacs ..."
        $install_cmd
    fi
}

setup_libvirtd() {
    local libvirtd_cmd=/usr/sbin/libvirtd
    if [ -x $libvirtd_cmd ]; then
        echo "Setup libvirtd ..."
        sudo $libvirtd_cmd -d
    fi
}

setup_sshd() {
    local sshd_cmd=/usr/sbin/sshd
    if [ -x $sshd_cmd ]; then
        echo "Setup sshd ..."
        sudo $sshd_cmd
    fi
}

setup_sleep() {
    echo "Setup sleep ..."
    sleep infinity
}

CMD=${1:-""}
case $CMD in
    init )
        setup_zzemacs
        setup_libvirtd
        setup_sshd
        setup_sleep
        ;;
    * )
        setup_zzemacs
        setup_sleep
        ;;
esac
