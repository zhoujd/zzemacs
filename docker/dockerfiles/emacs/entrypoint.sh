#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=${ZZEMACS_ROOT:-$HOME/zzemacs}
DAEMON_NAME=${DAEMON_NAME:-ee}

setup_common() {
    echo "Setup common ..."
}

setup_zzemacs() {
    echo "Start daemon ..."
    emacs --fg-daemon=$DAEMON_NAME
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
        setup_zzemacs
        ;;
    "help" )
        setup_help
        ;;
    * )
        exec "$@"
        ;;
esac
