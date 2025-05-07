#!/bin/bash
#set -x

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZZEMACS_ROOT=$(cd $SCRIPT_ROOT/../.. && pwd)
ZZEMACS_TOP=$(cd $ZZEMACS_ROOT/.. && pwd)

VER=${VER:-22.04}
IMG=${IMG:-zhoujd/ubuntu-${VER}-zzemacs}
TAG=${TAG:-base}
CTN_PREFIX=${CTN_PREFIX:-zzemacs}
CTN_NAME=${CTN_NAME:-$CTN_PREFIX-$TAG}
CTN_HOST=${CTN_HOST:-ubuntu-2204-zzemacs}
CTN_USER=${CTN_USER:-$USER}
CTN_HOME=${CTN_HOME:-/home/$CTN_USER}
SSH_HOST=${SSH_HOST:-localhost}
SSH_PORT=${SSH_PORT:-2222}
SSH_USER=${CTN_USER}
HOST_NAME=${HOST_NAME:-dockerhost}
HOST_IP=${HOST_IP:-host-gateway}
PROMPT=${PROMPT:-$(basename $0)}

## Use local X11 Server
X11_PARAM=(
    -e DISPLAY=$DISPLAY
    -v /tmp/.X11-unix:/tmp/.X11-unix
    -v $HOME/.Xauthority:$CTN_HOME/.Xauthority
)

RUN_PARAM=(
    --detach
    --name=${CTN_NAME}
    --restart=always
    --privileged=true
    --cap-add=ALL
    --add-host=$HOST_NAME:$HOST_IP
    -e GITHUB_TOKEN=$GITHUB_TOKEN
    -e GITLAB_TOKEN=$GITLAB_TOKEN
    -h $CTN_HOST
    -u $CTN_USER
    -p $SSH_PORT:22
    -v /tmp:/tmp
    -v /var/run/docker.sock:/var/run/docker.sock
    -v $HOME/work:$CTN_HOME/work
    -v $HOME/.ssh:$CTN_HOME/.ssh
    -v $ZZEMACS_ROOT:$CTN_HOME/zzemacs
)

EXEC_PARAM=(
    -e DISPLAY=$DISPLAY
    -e SHELL=/bin/bash
    -u $CTN_USER
)

EMACS_PARAM=(
    emacs
)

SHELL_PARAM=(
    bash
    -l
)

case $1 in
    start )
        docker run ${RUN_PARAM[@]} ${IMG}:${TAG} run
        ;;
    stop )
        docker stop ${CTN_NAME} 2>/dev/null
        docker rm ${CTN_NAME} 2>/dev/null
        ;;
    log )
        docker logs ${CTN_NAME} 2>/dev/null
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
        TERM=st ssh -X -l ${SSH_USER} ${SSH_HOST} -p ${SSH_PORT}
        ;;
    build )
        shift
        make -C dockerfiles $@
        ;;
    * )
        echo "Usage: ${PROMPT} {start|stop|log|status|emacs|shell|ssh|build}"
        ;;
esac
