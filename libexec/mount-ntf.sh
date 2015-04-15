#!/bin/sh


if [ ! $# = 3 ]; then
    echo "Use: `basename $0` <REMOTE_IP> <REMOTE_FOLDER> <LOCAL_FOLDER>"
    exit 1;
fi

REMOTE_IP=$1
REMOTE_FOLDER=$2
LOCAL_FOLDER=$3

Install_soft() {
	#install nfs-common for Ubuntu
	w=`cat /etc/issue | grep 'Ubuntu'`
	if [ "$w" != "" ]; then
		if ! which showmount > /dev/null; then
			sudo apt-get install -y nfs-common
		fi
	fi
}

Mount_net_fs() {
	sudo showmount -e ${REMOTE_IP}
	sudo mount -t nfs ${REMOTE_IP}:${REMOTE_FOLDER} ${LOCAL_FOLDER}
}

Install_soft
Mount_net_fs
