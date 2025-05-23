#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=${ZZEMACS_ROOT:-$HOME/zzemacs}
DAEMON_NAME=${DAEMON_NAME:-ee}

setup_common() {
    echo "Setup common ..."
    touch $HOME/.Xauthority
}

setup_emacs() {
    echo "Setup emacs ..."
    echo "Install emacs files"
    ln -sfvT ${ZZEMACS_ROOT}/.emacs $HOME/.emacs
    echo "Install font files"
    ln -sfvT ${ZZEMACS_ROOT}/font $HOME/.fonts
    echo "Install term rc files"
    ${ZZEMACS_ROOT}/misc/term/install.sh
    echo "Install debug rc files"
    ${ZZEMACS_ROOT}/misc/debug/install.sh all
    echo "Install git rc files"
    ${ZZEMACS_ROOT}/misc/gitconfig.d/install.sh
    echo "Install bash rc files"
    ${ZZEMACS_ROOT}/bin/bashrc-setup.sh
    echo "Start daemon"
    emacs --daemon=${DAEMON_NAME}
}

setup_dbus() {
    local dbus_cmd=/etc/init.d/dbus
    if [ -x $dbus_cmd ]; then
        echo "Setup dbus ..."
        sudo $dbus_cmd start
    fi
}

setup_ssh() {
    echo "Setup ssh ..."
    local ssh_cmd=/etc/init.d/ssh
    if [ -x $ssh_cmd ]; then
        sudo $ssh_cmd start
    fi
}

setup_sleep() {
    echo "Setup sleep ..."
    sleep infinity
}

setup_help() {
    echo "Usage: $0 {init|run|help}"
}

CMD=${1:-""}
case "$CMD" in
    "init" )
        setup_sleep
        ;;
    "run" )
        setup_common
        setup_dbus
        setup_ssh
        setup_emacs
        setup_sleep
        ;;
    "help" )
        setup_help
        ;;
    * )
        exec "$@"
        ;;
esac
