#!/bin/bash
#set -x

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
TOOL_ROOT=$SCRIPT_ROOT/script

case $1 in
    base )
        shift
        export TAG=base
        export SSH_PORT=10022
        export CTN_HOST=ubuntu-2004-zzemacs
        export PROMPT="$(basename $0) base"
        $TOOL_ROOT/tool.sh $@
        ;;
    dev )
        shift
        export TAG=dev
        export SSH_PORT=11022
        export CTN_HOST=ubuntu-2004-zzemacs
        export PROMPT="$(basename $0) dev"
        $TOOL_ROOT/tool.sh $@
        ;;
    status )
        docker ps | grep zzemacs
        ;;
    * )
        echo "Usage: $(basename $0) {base|dev|status}"
        ;;
esac
