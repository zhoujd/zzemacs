#!/bin/bash
#set -x

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
TOOL_ROOT=$SCRIPT_ROOT/script

case $1 in
    base )
        shift
        export TAG=base
        export PROMPT="$(basename $0) base"
        $TOOL_ROOT/tool.sh $@
        ;;
    dev )
        shift
        export TAG=dev
        export PROMPT="$(basename $0) dev"
        $TOOL_ROOT/tool.sh $@
        ;;
    status )
        docker ps | grep zzemacs
        ;;
    build )
        shift
        $TOOL_ROOT/tool.sh build $@
        ;;
    * )
        echo "Usage: $(basename $0) {base|dev|status|build}"
        ;;
esac
