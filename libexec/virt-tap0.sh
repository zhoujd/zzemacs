#!/bin/sh

##sudo apt-get install bridge-utils uml-utilities
##parameter: "-net nic -net tap,ifname=tap0,script=no,downscript=no"

TAP=tap0
BR=br0

tunctl -t $TAP -u root              # create a tap0, only root can access
brctl addif $BR $TAP                # add tap0 to virtural bridge br0
ifconfig $TAP 0.0.0.0 promisc up    # open tap0
brctl showstp $BR                   # show interfaces on br0
