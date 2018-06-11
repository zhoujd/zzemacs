#!/bin/sh

### Setup guide

## Install dependence
# apt-get install nfs-kernel-server
# yum install nfs-utils rpcbind

## Create share folder
# mkdir /home/user/share

## Update configure file
# vi /etc/exports
#> home/user/share *(rw,rync)
#> home/user/share 192.168.1.*(rw,rync)

## Start service on Ubuntu
#/etc/init.d/nfs-kernel-server restart
## Start service on CentOS
# service nfs restart

## Check nfs status
# netstat -lt
  
## Client on Linux
#mount -t nfs -o tcp,nolock 192.168.1.10:home/user/share /mnt
#mount -t nfs 192.168.1.10:home/user/share /mnt

## Client on Windows
# Control center -> open/close windows function -> NFS Client
# win+R->cmd
# mount 192.168.1.10:/home/user/share X:
# umount X:
# umount -a


## parameter check
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
