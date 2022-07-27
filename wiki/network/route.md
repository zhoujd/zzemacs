Route
=====

## How to Set Route Priorities

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
