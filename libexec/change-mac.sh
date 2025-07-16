#!/bin/sh

if [ $# != 2 ]; then
    echo "Usage: $0 <NIC> <MAC>"
    exit 1
fi

NIC=$1   # "enp0s31f6"
MAC=$2   # 12:34:56:78:90:ab

ip link set dev $NIC down
ip link set dev $NIC address $MAC
ip link set dev $NIC up

echo "Change $NIC MAC ADDR to $MAC done"
