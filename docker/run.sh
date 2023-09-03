#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)

REMOTE_HOST=$HOSTNAME
REMOTE_USER=$USER
REMOTE_HOME=$HOME
CTN=${CTN:-"zzemacs"}
IMG=${IMG:-"ubuntu-22.04-zzemacs:zach"}

## Use local X11 Server
#-e DISPLAY=$DISPLAY
#-e SHELL=/bin/bash
#-v /tmp/.X11-unix:/tmp/.X11-unix
#-v $HOME/.Xauthority:$REMOTE_HOME/.Xauthority
RUN_PARAM=(
    --privileged=true
    --cap-add=ALL
    -h $REMOTE_HOST
    -u $REMOTE_USER
    -p 2222:22
    -v /var/run/docker.sock:/var/run/docker.sock
    -v $ZZEMACS_ROOT:$REMOTE_HOME/zzemacs
    -v $ZZEMACS_ROOT/font:$REMOTE_HOME/.fonts
    -v $ZZEMACS_ROOT/.emacs:$REMOTE_HOME/.emacs
)

EXEC_PARAM=(
    -e DISPLAY=$DISPLAY
    -e SHELL=/bin/bash
    -u $REMOTE_USER
)

EMACS_PARAM=(
    -nw
)

case $1 in
    start )
        docker run -d --name=${CTN} ${RUN_PARAM[@]} ${IMG} &> /dev/null
        ;;
    stop )
        docker stop ${CTN} &> /dev/null
        docker rm ${CTN} &> /dev/null
        ;;
    emacs )
        docker exec -it ${EXEC_PARAM[@]} ${CTN} emacs ${EMACS_PARAM[@]}
        ;;
    shell )
        docker exec -it ${EXEC_PARAM[@]} ${CTN} bash -l
        ;;
    status )
        docker ps | grep ${CTN}
        ;;
    * )
        echo "Usage: $(basename $0) {start|stop|shell|emacs|status}"
        ;;
esac
