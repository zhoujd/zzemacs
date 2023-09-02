#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)

REMOTE_HOST=$HOSTNAME
REMOTE_USER=$USER
REMOTE_HOME=$HOME

RUN_PARAM=(
    --privileged
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

CTN_NAME=zzemacs-001

case $1 in
    start )
        docker run -d --name=${CTN_NAME} ${RUN_PARAM[@]} ubuntu:zach
        ;;
    stop )
        docker stop ${CTN_NAME}
        docker rm ${CTN_NAME}
        ;;
    emacs )
        docker exec -it ${CTN_NAME} emacs ${EMACS_PARAM[@]}
        ;;
    shell )
        docker exec -it ${CTN_NAME} bash -l
        ;;
    * )
        echo "Usage: $(basename $0) {start|stop|shell|emacs}"
        ;;
esac
