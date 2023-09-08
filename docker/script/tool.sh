#!/bin/bash
#set -x

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/../.. && pwd)
ZZEMACS_TOP=$(cd $ZZEMACS_ROOT/.. && pwd)

IMG=${IMG:-ubuntu-2004-zzemacs}
TAG=${TAG:-base}
CTN_PREFIX=${CTN_PREFIX:-zzemacs}
CTN_NAME=${CTN_NAME:-$CTN_PREFIX-$TAG}
REMOTE_HOST=${REMOTE_HOST:-$IMG}
REMOTE_USER=${REMOTE_USER:-$USER}
REMOTE_HOME=${REMOTE_HOME:-/home/$REMOTE_USER}
SSH_HOST=${SSH_HOST:-localhost}
SSH_PORT=${SSH_PORT:-10022}
SSH_USER=${REMOTE_USER}
HOST_NAME=${HOST_NAME:-myhost}
HOST_IP=${HOST_IP:-host-gateway}
HELP_PRFIX=${HELP_PRFIX:-$(basename $0)}

## Use local X11 Server
X11_PARAM=(
    -e DISPLAY=$DISPLAY
    -v /tmp/.X11-unix:/tmp/.X11-unix
    -v $HOME/.Xauthority:$REMOTE_HOME/.Xauthority
)

RUN_PARAM=(
    --privileged=true
    --cap-add=ALL
    --add-host=$HOST_NAME:$HOST_IP
    -h $REMOTE_HOST
    -u $REMOTE_USER
    -p $SSH_PORT:22
    -v /dev:/dev
    -v /var/run/docker.sock:/var/run/docker.sock
    -v /etc/security/limits.conf:/etc/security/limits.conf
    -v $ZZEMACS_TOP/lab:$REMOTE_HOME/lab
    -v $ZZEMACS_ROOT:$REMOTE_HOME/zzemacs
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
        docker run -d --name=${CTN_NAME} ${RUN_PARAM[@]} ${IMG}:${TAG}
        ;;
    stop )
        docker stop ${CTN_NAME} && docker rm ${CTN_NAME}
        ;;
    status )
        docker ps | grep ${CTN_NAME}
        ;;
    emacs )
        docker exec -it ${EXEC_PARAM[@]} ${CTN_NAME} ${EMACS_PARAM[@]}
        ;;
    shell )
        docker exec -it ${EXEC_PARAM[@]} ${CTN_NAME} ${SHELL_PARAM[@]}
        ;;
    ssh )
        TERM=xterm ssh -X -l ${SSH_USER} ${SSH_HOST} -p ${SSH_PORT}
        ;;
    build )
        make -C dockerfiles
        ;;
    * )
        echo "Usage: ${HELP_PRFIX} {start|stop|status|emacs|shell|ssh|build}"
        ;;
esac
