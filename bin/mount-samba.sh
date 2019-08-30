#!/bin/bash

if [ -z "$SMB_SOURCE" ] || [ -z "$SMB_TARGET" ] ; then
    echo "please set following"
    echo "export SMB_SOURCE="
    echo "export SMB_TARGET="
    exit 1
fi

SOURCE=$SMB_SOURCE
TARGET=$SMB_TARGET

start() {
    echo "mount $SOURCE -> $TARGET"
    sudo mount -t cifs -o user=$USER,uid=`id -u $USER`,gid=`id -g $USER`,iocharset=utf8,file_mode=0644,dir_mode=0644,noperm \
         $SOURCE \
         $TARGET
}

stop() {
    echo "umount $TARGET"
    sudo umount $TARGET
}

status() {
    mount | grep $TARGET
}

case $1 in
    start )
        start
        ;;
    stop )
        stop
        ;;
    restart )
        stop
        start
        ;;
    status )
        status
        ;;
    * )
        echo "$0 {start|stop|restart|status}"
        exit 1
        ;;
esac
