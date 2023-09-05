#!/bin/bash
#set -x

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)
ZZEMACS_TOP=$(cd $ZZEMACS_ROOT/.. && pwd)

REMOTE_HOST=$HOSTNAME
REMOTE_USER=$USER
REMOTE_HOME=$HOME
SSH_HOST=${SSH_HOST:-localhost}
SSH_PORT=${SSH_PORT:-10022}
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
    --env-file=env.sh
    -h $REMOTE_HOST
    -u $REMOTE_USER
    -p $SSH_PORT:22
    -v /var/run/docker.sock:/var/run/docker.sock
    -v $ZZEMACS_ROOT:$REMOTE_HOME/zzemacs
    -v $ZZEMACS_TOP/work:$REMOTE_HOME/work
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
        docker run -d --name=${CTN} ${RUN_PARAM[@]} ${IMG}
        ;;
    stop )
        docker stop ${CTN} && docker rm ${CTN}
        ;;
    status )
        docker ps | grep ${CTN}
        ;;
    emacs )
        docker exec -it ${EXEC_PARAM[@]} ${CTN} emacs ${EMACS_PARAM[@]}
        ;;
    shell )
        docker exec -it ${EXEC_PARAM[@]} ${CTN} bash -l
        ;;
    ssh )
        ssh -X $SSH_HOST -p $SSH_PORT
        ;;
    * )
        echo "Usage: $(basename $0) {start|stop|status|emacs|shell|ssh}"
        ;;
esac
