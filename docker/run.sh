#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)

RUN_PARAM=(
    -e DISPLAY=$DISPLAY
    -h $HOSTNAME
    -v /tmp/.X11-unix:/tmp/.X11-unix
    -v $HOME/.Xauthority:/home/zach/.Xauthority
    -v $ZZEMACS_ROOT/.emacs:/home/zach/.emacs
    -v $ZZEMACS_ROOT/font:/home/zach/.fonts
    -v $ZZEMACS_ROOT:/home/zach/zzemacs
)

docker run ${RUN_PARAM[@]} ubuntu:zach emacs
