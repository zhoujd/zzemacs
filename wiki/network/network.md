Network
=======

## Remove Netplan from Ubuntu Bionic Beaver (18.04)

    ## Install ifupdown
    $ sudo apt update
    $ sudo apt install -y ifupdown

    ## Delete all of the Netplan configuration files
    $ sudo rm -rf /etc/netplan/*.yml

    ## Replace configuration files
    $ cat /etc/network/interfaces
    source /etc/network/interfaces.d/*.cfg

    auto enp10s0
    iface enp10s0 inet static
    address 192.168.1.162
    netmask 255.255.255.0
    gateway 192.168.1.100
    dns-nameservers 1.0.0.1,1.1.1.1

    $ sudo systemctl restart networking
    $ sudo /etc/init.d/networking restart

## Install Netplan on Ubuntu

    $ sudo apt update
    $ sudo apt install netplan
    $ ip a
    $ sudo nano /etc/netplan/50-cloud-init.yaml
    network:
         Version: 2
         Renderer: NetworkManager/ networkd
         ethernets:
            DEVICE_NAME:
               Dhcp4: yes/no
               Addresses: [IP_ADDRESS/NETMASK]
               Gateway: GATEWAY
               Nameservers:
                  Addresses: [NAMESERVER_1, NAMESERVER_2]

    $ sudo netplan try
    $ sudo netplan apply

    ## The gateway is also set correctly
    $ route -n

    ## The DNS server is set correctly as well
    $ systemd-resolve --status ens33


## Restart the network service

    $ sudo systemctl restart networking
    $ sudo /etc/init.d/networking restart
    $ sudo systemctl restart network-manager
    $ sudo systemctl restart systemd-networkd

## How to Manually Set Your IP

    ## Using ifconfig
    ## 1. Set Your IP Address
    $ sudo ifconfig eth0 192.168.1.5 netmask 255.255.255.0 up
    ## 2. Set Your Default Gateway
    $ route add default gw 192.168.1.1
    ## 3. Set Your DNS Server
    $ echo "nameserver 1.1.1.1" > /etc/resolv.conf

    ## Using ip
    ## Show your IP using ip
    $ ip addr show
    ## Bring an interface up or down using ip
    $ ip link set eth1 up
    $ ip link set eth1 down
    ## Showing your routing using ip
    $ ip route show

## How can I renew or release an IP in Linux for eth0

    $ sudo dhclient -v -r eth0
    $ sudo dhclient -v eth0

## Change hostname on Ubuntu server

    $ nano /etc/cloud/cloud.cfg
    # This will cause the set+update hostname module to not operate (if true)
    # For change the hostname, from "false" to "true"
    preserve_hostname: true

## DNS configuration

    ## configuration
    $ cat /run/systemd/resolve/stub-resolv.conf
    $ cat /run/resolvconf/resolv.conf

    $ sudo apt install -y resolvconf

    ## remove systemd-resolved
    $ sudo systemctl status systemd-resolved.service
    $ sudo systemctl disable systemd-resolved.service
    $ sudo systemctl stop systemd-resolved.service

    ## use resolvconf
    $ sudo nano /usr/lib/NetworkManager/conf.d/10-dns-resolved.conf
    dns=resolvconf

    ## restart NetworkManager
    $ sudo rm /etc/resolv.conf
    $ sudo nano /etc/NetworkManager/NetworkManager.conf
    dns=default
    rc-manager=resolvconf

    $ sudo service Networkmanager restart

## NetworkManager

    https://wiki.archlinux.org/index.php/NetworkManager


## tsocks

    $ sudo apt install tsocks
    ## config tsocks
    $ cat /etc/tsocks.conf
    $ cat ~/.tsocks.conf
    ## tsocks.conf
    local = 192.168.0.0/255.255.0.0
    local = 134.134.0.0/255.255.0.0
    local = 10.0.0.0/255.0.0.0
    server = 10.109.19.69
    server_type = 5
    server_port = 1080

## 10 useful commands when debugging network issues in Linux
    https://medium.com/@janethavishka/10-useful-commands-when-debugging-network-issues-in-linux-71d082fca5a1

    1) ping(Packet Internet Groper)
    2) telnet(Teletype Network)
    3) netstat(Network Statistics)
    4) ifconfig(Interface Configuration)
    5) traceroute
    6) route
    7) arp(Address Resolution Protocol)
    8) dig(Domain Information Groper)
    9) nslookup(Name Server Lookup)
    10) nmap(Network Map)

## Test connectivity to a specific TCP service listening on your host

    $ sudo apt install iputils-ping netcat-openbsd
    $ nc -vz host.minikube.internal 8000
    Connection to host.minikube.internal 8000 port [tcp/*] succeeded!

## inxi :: a full featured system information script

    ## https://smxi.org/docs/inxi.htm
    ##
    $ sudo apt install inxi
    $ inxi -i
    $ inxi -b
