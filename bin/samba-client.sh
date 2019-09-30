#!/bin/bash

if [ -z "$SMB_SOURCE" ] || [ -z "$SMB_TARGET" ] ; then
    echo "please set following"
    echo "export SMB_SOURCE="
    echo "export SMB_TARGET="
    echo "export SMB_USER="
    echo "export SMB_PASSWD="
    echo "export MNT_USER="
    exit 1
fi

start() {
    echo "mount $SMB_SOURCE -> $SMB_TARGET"
    MNT_UID=$(id -u $MNT_USER)
    MNT_GID=$(id -g $MNT_USER)
    sudo mount -t cifs \
               -o user=$SMB_USER,password=$SMB_PASSWD,uid=$MNT_UID,gid=$MNT_UID,iocharset=utf8,file_mode=0644,dir_mode=0644,noperm \
               $SMB_SOURCE \
               $SMB_TARGET
}

stop() {
    echo "umount $SMB_TARGET"
    sudo umount $SMB_TARGET
}

status() {
    mount | grep $SMB_TARGET
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
