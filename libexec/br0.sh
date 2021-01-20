#!/bin/sh

##sudo apt install bridge-utils
##sudo yum install bridge-utils

if [ $# != 2 ] ; then
    echo "Usage: `basename $0` <NIC> <BRIDGE>"
    echo "       NIC: find it via ifconfig"
    echo "       BRIDGE: xenbr0 or br0"
    exit 1;
fi

if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

eth0=$1
br0=$2

ifconfig $eth0 down                  # close eth0 first
brctl addbr $br0 || true             # add virtual bridge br0
brctl addif $br0 $eth0               # add interface eth0 on br0
brctl stp $br0 off                   # only on bridge, so close generate tree protocol
brctl setfd $br0 1                   # set br0 delaytime of forward
brctl sethello $br0 1                # set br0 hello time
ifconfig $br0 0.0.0.0 promisc up     # open br0
ifconfig $eth0 0.0.0.0 promisc up    # open eth0
dhclient $br0                        # br0 get IP address from DHCP server
brctl show $br0                      # show virtual bridge list
brctl showstp $br0                   # show interface information on br0
