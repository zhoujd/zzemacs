#!/bin/bash

SOURCE=//u18-kbl-zz/zach
TARGET=/zach/share

USER=jiandon

start() {
    echo "mount $SOURCE -> $TARGET"
    sudo mount -t cifs -o user=$USER,uid=`id -u $USER`,gid=`id -g $USER`,iocharset=utf8,file_mode=0777,dir_mode=0777,noperm \
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
