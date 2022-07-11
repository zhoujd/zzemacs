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
    ## https://smxi.org/docs/inxi-installation.htm
    $ sudo apt install inxi
    $ inxi -i
    $ inxi -b

## Macvlan and IPvlan basics

    ## https://cizixs.com/2017/02/14/network-virtualization-macvlan/
    ## https://sreeninet.wordpress.com/2016/05/29/macvlan-and-ipvlan/
    $ sudo ip link add mymacvlan1 link eth0 type macvlan mode bridge
    $ sudo ip link add mymacvlan2 link eth0 type macvlan mode bridge
    $ sudo ifconfig mymacvlan1 up
    $ sudo ifconfig mymacvlan2 up

    ## macvlan with subinterface
    $ sudo vconfig add eth2 10
    $ sudo vconfig add eth2 20
    $ sudo ip link add mymacvlan1 link eth2.10 type macvlan mode bridge
    $ sudo ip link add mymacvlan2 link eth2.10 type macvlan mode bridge
    $ sudo ifconfig mymacvlan1 up
    $ sudo ifconfig mymacvlan2 up
    $
    $ sudo ip link add mymacvlan3 link eth2.20 type macvlan mode bridge
    $ sudo ip link add mymacvlan4 link eth2.20 type macvlan mode bridge
    $ sudo ifconfig mymacvlan3 up
    $ sudo ifconfig mymacvlan4 up

    ## ipvlan need kernel support > 4.2
    ## ipvlan is similar to macvlan with the difference being that the endpoints have the same mac address
    $ sudo ip link add myipvlan1 link enp0s3 type ipvlan mode l2
    $ sudo ifconfig myipvlan1 up
    $ sudo ip link add myipvlan2 link enp0s3 type ipvlan mode l2
    $ sudo ifconfig myipvlan2 up

## Use bridge connect namespace

    $ ip link add br0 type bridge
    $ ip link set dev br0 up

    $ ip link add type veth

    $ ip link set dev veth1 netns net0
    $ ip netns exec net0 ip link set dev veth1 name eth0
    $ ip netns exec net0 ip addr add 10.0.1.1/24 dev eth0
    $ ip netns exec net0 ip link set dev eth0 up

    $ ip link set dev veth0 master br0
    $ ip link set dev veth0 up
    $ bridge link

## Send a file over TCP port 9899 from host2 (client) to host1 (server).

    ## https://en.wikipedia.org/wiki/Netcat
    user@HOST1$ ncat -l 9899 > outputfile
    user@HOST2$ ncat HOST1 9899 < inputfile
    ## Transfer in the other direction, turning Ncat into a “one file” server.
    user@HOST1$ ncat -l 9899 < inputfile
    user@HOST2$ ncat HOST1 9899 > outputfile

## Setting up taps on Linux

    ## https://wiki.qemu.org/Documentation/Networking
    # modprobe tun tap                  # unnecessary if tun/tap is built-in
    # ip link add br0 type bridge
    # ip tuntap add dev tap0 mode tap
    # ip link set dev tap0 master br0   # set br0 as the target bridge for tap0
    # ip link set dev eth0 master br0   # set br0 as the target bridge for eth0
    # ip link set dev br0 up

    ## reassigning the physical device's addresses for the bridge to be usable
    # ip address delete $PREFIX dev eth0
    # ip address add $PREFIX dev br0
    # ip route add default via $ROUTE dev br0

    ## -netdev TYPE,id=NAME,...
    # qemu -netdev tap,id=tap0 linux.img
    # qemu -net tap -net tap0 linux.img

## Private Address Space

    ## https://www.rfc-editor.org/rfc/rfc1918
    10.0.0.0        -   10.255.255.255  (10/8 prefix)
    172.16.0.0      -   172.31.255.255  (172.16/12 prefix)
    192.168.0.0     -   192.168.255.255 (192.168/16 prefix)

## Use ifconfig to switch an interface to DHCP and if not

    ## static -> dhcp
    $ ifconfig eth0 0.0.0.0 0.0.0.0 && dhclient
    ## dhcp -> static
    $ killall dhclient && ifconfig eth0 10.0.1.22 netmask 255.255.255.0

## Setting up IP Aliasing

    ## Setting up IP Aliasing on A Linux Machine Mini−HOWTO
    ## https://tldp.org/HOWTO/pdf/IP-Alias.pdf
    ## 1. Load the IP Alias module (you can skip this step if you compiled the module into the kernel)
    $ sudo /sbin/insmod /lib/modules/`uname −r`/ipv4/ip_alias.o

    ## 2. Setup the loopback, eth0, and all the IP addresses beginning with the main IP address for the eth0 interface
    $ sudo /sbin/ifconfig lo 127.0.0.1
    $ sudo /sbin/ifconfig eth0 up
    $ sudo /sbin/ifconfig eth0 172.16.3.1
    $ sudo /sbin/ifconfig eth0:0 172.16.3.10
    $ sudo /sbin/ifconfig eth0:1 172.16.3.100

    ## 3. Setup the routes. First route the loopback, then the net, and finally, the various IP addresses starting with the default (originally allocated) one
    $ sudo /sbin/ifconfig lo 127.0.0.1
    $ sudo /sbin/ifconfig eth0 up
    $ sudo /sbin/ifconfig eth0 172.16.3.1
    $ sudo /sbin/ifconfig eth0:0 172.16.3.10
    $ sudo /sbin/ifconfig eth0:1 172.16.3.100

    ## And /proc/net/aliases:
    device family address
    eth0:0 2 172.16.3.10
    eth0:1 2 172.16.3.100

    ## And /proc/net/alias_types:
    type name n_attach
    2 ip 2

    ## My /etc/rc.d/rc.local: (edited to show the relevant portions)
    # setting up IP alias interfaces
    echo "Setting 172.16.3.1, 172.16.3.10, 172.16.3.100 IP Aliases ..."
    /sbin/ifconfig lo 127.0.0.1
    /sbin/ifconfig eth0 up
    /sbin/ifconfig eth0 172.16.3.1
    /sbin/ifconfig eth0:0 172.16.3.10
    /sbin/ifconfig eth0:1 172.16.3.100
    # setting up the routes
    echo "Setting IP routes ..."
    /sbin/route add −net 127.0.0.0
    /sbin/route add −net 172.16.3.0 dev eth0
    /sbin/route add −host 172.16.3.1 eth0
    /sbin/route add −host 172.16.3.10 eth0:0
    /sbin/route add −host 172.16.3.100 eth0:1
    /sbin/route add default gw 172.16.3.200

## Running in a network namespace

    $ ip link set dev MY_DEVICE down
    $ ip link set dev MY_DEVICE netns MY_NAMESPACE
    $ ip netns exec MY_NAMESPACE NetworkManager
    $ ip netns exec MY_NAMESPACE killall NetworkManager

## How can I clear the IP address

    $ ip addr del 10.22.30.44/16 dev eth0
    $ ip addr flush dev eth0
    $ ifconfig eth0 0.0.0.0

    ## To remove all adreses from all interfaces
    $ for i in $(ls /sys/class/net/) ; do
         /usr/sbin/ip addr flush $i &
      done

## Creating dummy interfaces on Linux

    ## Immediately create two dummyX interfaces
    $ sudo modprobe -v dummy numdummies=2
    $ lsmod | grep dummy
    $ ifconfig -a | grep dummy

    ## Add or remove an IP address
    $ sudo ip addr add 192.168.1.150/24 dev dummy0
    $ sudo ip addr del 192.168.1.150/24 dev dummy0

    ## Change the MAC address
    $ sudo ip link set dummy0 address 00:00:00:11:11:11

    ## Interfaces are added or removed
    $ sudo ip link add dummy2 type dummy
    $ sudo ip link del dummy2 type dummy

    ## Unload the dummy module (dummy interfaces will be deleted automatically)
    $ sudo rmmod dummy

    ## Add to the /etc/modules file (one dummy0 interface will be created at startup)
    $ cat /etc/modules
      dummy

    ## Create for example two interfaces or more
    $ cat /etc/rc.local
    $ modprobe -v dummy numdummies=2

    ## Or create a dummy.conf file
    $ sudo su
    $ echo "options dummy numdummies=2" > /etc/modprobe.d/dummy.conf

    ## System starts on the dummy interface there is an IP address
    $ sudo nano /etc/network/interfaces
    auto dummy0
    iface dummy0 inet static
    address 192.168.1.150
    netmask 255.255.255.0

## Dummy interface

    ## Dummy interface is mostly used for testing and debugging
    $ ip link add dummy1 type dummy
    $ ip addr add 1.1.1.1/24 dev dummy1
    $ ip link set dummy1 up

## Ping using specific gateway interface

    ## Pass the -I option
    $ ping -I interface destination
    $ ping -I eth0 www.cyberciti.biz
    $ ping -I tun0 1.1.1.1
    $ ping -I br0 8.8.8.8
