#!/bin/bash
## systemctl --user list-units --type service
## systemctl --user edit tmux.service
## systemctl --user daemon-reload
## systemctl --user restart tmux.service

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
SERV_ROOT=~/.config/systemd/user

install() {
    echo "Install tmux daemon"
    mkdir -p $SERV_ROOT
    cp -v $SCRIPT_ROOT/tmux.service $SERV_ROOT/tmux.service
    systemctl daemon-reload --user
    systemctl --user enable --now tmux
}

uninstall() {
    echo "Uninstall tmux daemon"
    systemctl --user disable --now tmux
}

stop() {
    echo "Stop tmux daemon"
    systemctl --user stop --now tmux
}

start() {
    echo "Start tmux daemon"
    systemctl --user start --now tmux
}

restart() {
    echo "Restart tmux daemon"
    systemctl --user restart --now tmux
}

status() {
    echo "Status of tmux daemon"
    systemctl status --user tmux
    ps -ef | grep tmux | grep daemon | grep -v tmux-daemon

}

case $1 in
    install )
        install
        ;;
    uninstall )
        uninstall
        ;;
    start )
        start
        ;;
    stop )
        stop
        ;;
    restart )
        restart
        ;;
    status )
        status
        ;;
     * )
        echo "Usage: $(basename $0) {install|uninstall|start|stop|restart|status}"
        ;;
esac
