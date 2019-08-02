network
=======

1. Remove Netplan from Ubuntu Bionic Beaver (18.04)
   
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
         
        
2. Install Netplan on Ubuntu

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
        

3. Restart the network service

        $ sudo systemctl restart networking
        $ sudo /etc/init.d/networking restart
        $ sudo systemctl restart network-manager
        $ sudo systemctl restart system-networkd

4. How to Manually Set Your IP

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
