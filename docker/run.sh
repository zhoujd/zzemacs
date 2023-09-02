#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)

REMOTE_HOST=$HOSTNAME
REMOTE_USER=$USER
REMOTE_HOME=$HOME

RUN_PARAM=(
    -e DISPLAY=$DISPLAY
    -e SHELL=/bin/bash
    -h $REMOTE_HOST
    -u $REMOTE_USER
    -v /tmp/.X11-unix:/tmp/.X11-unix
    -v $HOME/.Xauthority:$REMOTE_HOME/.Xauthority
    -v /var/run/docker.sock:/var/run/docker.sock
    -v $ZZEMACS_ROOT:$REMOTE_HOME/zzemacs
    -v $ZZEMACS_ROOT/font:$REMOTE_HOME/.fonts
    -v $ZZEMACS_ROOT/.emacs:$REMOTE_HOME/.emacs
)

EMACS_PARAM=(
    -nw
)

docker run -it --rm --name=zzemacs-001 ${RUN_PARAM[@]} ubuntu:zach emacs ${EMACS_PARAM[@]}
