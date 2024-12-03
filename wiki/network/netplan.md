netplan
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

## How to configure network bridges

    ## https://netplan.readthedocs.io/en/stable/examples/#how-to-configure-network-bridges
    ## https://github.com/canonical/netplan/tree/main/examples#configuring-network-bridges
    ## https://www.baeldung.com/linux/netplan-bridge-two-interfaces
    ## Use the following configuration to create a simple bridge consisting of a single device that uses DHCP:
    # Let networkd manage all devices on this system
    network:
      version: 2
      renderer: networkd
      ethernets:
        enp3s0:
          dhcp4: no
      bridges:
        br0:
          dhcp4: yes
          interfaces:
            - enp3s0


    # Let NetworkManager manage all devices on this system
    network:
      version: 2
      renderer: NetworkManager
      ethernets:
        enp0s3:
          dhcp4: no

      bridges:
        br0:
          dhcp4: yes
          interfaces:
            - enp0s3


## Creating a Network Bridge Using a Static IP

    ## https://www.baeldung.com/linux/netplan-bridge-two-interfaces
    network:
      version: 2
      renderer: NetworkManager
      ethernets:
        enp0s3:
          dhcp4: no
      bridges:
        br0:
          dhcp4: no
          interfaces:
            - enp0s3
          addresses:
            - 192.168.137.100/24
          nameservers:
             search: []
             addresses: [8.8.8.8,1.1.1.1]
          routes:
             - to: default
               via: 192.168.137.1
