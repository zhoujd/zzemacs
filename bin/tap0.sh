#!/bin/sh

##sudo apt-get install bridge-utils uml-utilities
##parameter: "-net nic -net tap,ifname=tap0,script=no,downscript=no"

tunctl -t tap0 -u root              # create a tap0, only root can access
brctl addif br0 tap0                # add tap0 to virtural bridge br0
ifconfig tap0 0.0.0.0 promisc up    # open tap0
brctl showstp br0                   # show interfaces on br0

