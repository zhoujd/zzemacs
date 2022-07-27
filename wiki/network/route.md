Route
=====

## How to Set Route Priorities

    ## http://0pointer.de/lennart/projects/ifmetric/
    ## The default metric for a route in the Linux kernel is 0, meaning the highest priority.
    $ sudo apt install ifmetric
    $ ifmetric <interface name> [metric value]

## route or to ip route

    ## Displaying existing routes
    $ route
    $ ip route  show

    ## Adding new routes
    ## Syntax is: route add -net <network_address> gw <gatewayaddr> <interfacename>
    $ sudo route add -net 10.0.2.0/24 gw 192.168.0.1 enp0s3
    ## Syntax is: ip route add <network you want to connect to> via <ip used to reach the network> dev <interface name>
    $ sudo ip route add 10.0.2.0/24 via 192.168.0.1 dev enp0s3

    ## Removing routes
    $ sudo route del -net 10.0.2.0/24 gw 192.168.0.1 enp0s3
    $ sudo ip route del 10.0.2.0/24 via 192.168.0.1 dev enp0s3

    ## Adding a new default gateway
    ## Configuring traffic to flow to a gateway
    $ sudo route add default gw 192.168.0.1
    $ sudo ip route add default via 192.168.0.1

    ## Show how the linux kernel classifies the route
    $ ip route get 1.2.3.4

## Permanently adding static route (RHEL, Fedora, CentOS)

    $ vim /etc/sysconfig/network-scripts/route-enps03
    10.0.2.0/32 via 192.168.43.1
    10.0.2.15  via 192.168.43.1
    $ sudo systemctl restart NetworkManager

## Permanently adding static route (Ubuntu / Debian)

    $ sudo vim /etc/network/interfaces
    up route add -net 10.0.2.0 netmask 255.255.255.0 gw 192.168.43.1 dev enp0s3
    $ sudo ifdown enp0s3 && sudo ifup enp0s3

## To reject network packets to a particular host or to a network

    $ sudo route add -host 10.10.2.20 reject
    $ route add -net 10.10.2.0 netmask 255.0.0.0 reject

## 3 type route

    ## Host
    Destination     Gateway         Genmask         Flags Metric Ref    Use Iface
    -----------     -----------     --------------  ----- ------ ---    --- ------
    10.0.0.10       192.168.1.1     255.255.255.255 UH    100    0        0 eth0

    ## Network
    Destination    Gateway       Genmask        Flags    Metric    Ref     Use    Iface
    -----------    -------       -------        -----    -----     ---     ---    -----
    192.19.12.0   192.168.1.1    255.255.255.0   UG      0         0       0      eth0

    ## Default
    Destination    Gateway       Genmask    Flags     Metric    Ref    Use    Iface
    -----------    -------       -------    -----    ------     ---    ---    -----
    default       192.168.1.1    0.0.0.0    UG        0         0      0      eth0

## route examples

    ## https://ivanzz1001.github.io/records/post/linuxops/2018/11/14/linux-route
    ## Add host route
    $ route add -host 192.168.1.2 dev eth0
    $ route add -host 10.20.30.148 gw 10.20.30.40

    ## Add network route
    $ route add -net 10.20.30.40 netmask 255.255.255.248 eth0           #To 10.20.30.40
    $ route add -net 10.20.30.48 netmask 255.255.255.248 gw 10.20.30.41 #To 10.20.30.48
    $ route add -net 192.168.1.0/24 eth1

    ## add default route
    $ route add default gw 192.168.1.1

    ## Delete route
    $ route del -host 192.168.1.2 dev eth0:0
    $ route del -host 10.20.30.148 gw 10.20.30.40
    $ route del -net 10.20.30.40 netmask 255.255.255.248 eth0
    $ route del -net 10.20.30.48 netmask 255.255.255.248 gw 10.20.30.41
    $ route del -net 192.168.1.0/24 eth1
    $ route del default gw 192.168.1.1

    ## reject
    $ route add -net 224.0.0.0 netmask 240.0.0.0 reject
