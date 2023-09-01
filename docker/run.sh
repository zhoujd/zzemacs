#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)

REMOTE_HOST=$HOSTNAME
REMOTE_USER=zach
REMOTE_HOME=/home/$REMOTE_USER

RUN_PARAM=(
    -e DISPLAY=$DISPLAY
    -e SHELL=/bin/bash
    -h $REMOTE_HOST
    -u $REMOTE_USER
    -v /tmp/.X11-unix:/tmp/.X11-unix
    -v $HOME/.Xauthority:$REMOTE_HOME/.Xauthority
    -v $ZZEMACS_ROOT:$REMOTE_HOME/zzemacs
    -v $ZZEMACS_ROOT/font:$REMOTE_HOME/.fonts
    -v $ZZEMACS_ROOT/.emacs:$REMOTE_HOME/.emacs
)

docker run ${RUN_PARAM[@]} ubuntu:zach emacs
