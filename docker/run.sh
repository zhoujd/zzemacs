#!/bin/bash
#set -x

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/.. && pwd)
ZZEMACS_TOP=$(cd $ZZEMACS_ROOT/.. && pwd)

CTN=${CTN:-zzemacs}
IMG=${IMG:-ubuntu-2004-zzemacs}
TAG=${TAG:-dev}
REMOTE_HOST=${REMOTE_HOST:-$IMG}
REMOTE_USER=${REMOTE_USER:-$USER}
REMOTE_HOME=${REMOTE_HOME:-/home/$REMOTE_USER}
SSH_HOST=${SSH_HOST:-localhost}
SSH_PORT=${SSH_PORT:-10022}
SSH_USER=${REMOTE_USER}
DID_SOCK=${DID_SOCK:-/var/run/docker.sock}
MYHOST_NAME=myhost
MYHOST_IP=host-gateway
MYHOST_USER=${REMOTE_USER}

## Use local X11 Server
X11_PARAM=(
    -e DISPLAY=$DISPLAY
    -v /tmp/.X11-unix:/tmp/.X11-unix
    -v $HOME/.Xauthority:$REMOTE_HOME/.Xauthority
)

RUN_PARAM=(
    --privileged=true
    --cap-add=ALL
    --add-host=$MYHOST_NAME:$MYHOST_IP
    -h $REMOTE_HOST
    -u $REMOTE_USER
    -p $SSH_PORT:22
    -v /dev:/dev
    -v $DID_SOCK:$DID_SOCK
    -v $ZZEMACS_ROOT:$REMOTE_HOME/zzemacs
    -v $ZZEMACS_TOP/lab:$REMOTE_HOME/lab
)

EXEC_PARAM=(
    -e DISPLAY=$DISPLAY
    -e SHELL=/bin/bash
    -u $REMOTE_USER
)

EMACS_PARAM=(
    emacs
    -nw
)

SHELL_PARAM=(
    bash
    -l
)

case $1 in
    start )
        docker run -d --name=${CTN} ${RUN_PARAM[@]} ${IMG}:${TAG}
        ;;
    stop )
        docker stop ${CTN} && docker rm ${CTN}
        ;;
    status )
        docker ps | grep ${CTN}
        ;;
    emacs )
        docker exec -it ${EXEC_PARAM[@]} ${CTN} ${EMACS_PARAM[@]}
        ;;
    shell )
        docker exec -it ${EXEC_PARAM[@]} ${CTN} ${SHELL_PARAM[@]}
        ;;
    ssh )
        ssh -X -l $SSH_USER $SSH_HOST -p $SSH_PORT
        ;;
    host )
        ssh -l $MYHOST_USER $MYHOST_IP
        ;;
    * )
        echo "Usage: $(basename $0) {start|stop|status|emacs|shell|ssh|host}"
        ;;
esac
