Bridge
======

https://www.hiroom2.com/2018/05/08/ubuntu-1804-bridge-en/

## bridge-utils + DHCP
```
$ INTERFACE=ens3
$ sudo apt install -y bridge-utils
$ cat <<EOF | sudo tee /etc/network/interfaces
auto lo
iface lo inet loopback
auto br0
iface br0 inet dhcp
      bridge_ports ${INTERFACE}
      bridge_stp off
      bridge_maxwait 0
EOF
$ sudo reboot
```

## bridge-utils + Static IP Address
```
$ INTERFACE=ens3
$ sudo apt install -y bridge-utils
$ cat <<EOF | sudo tee /etc/network/interfaces
auto lo
iface lo inet loopback
auto br0
iface br0 inet static
      address 192.168.11.250
      netmask 255.255.255.0
      network 192.168.11.0
      broadcast 192.168.11.255
      gateway 192.168.11.1
      dns-nameservers 192.168.11.2
      dns-search hiroom2.com
      bridge_ports ${INTERFACE}
      bridge_stp off
      bridge_maxwait 0
EOF
$ sudo sed -i /etc/systemd/resolved.conf \
        -e 's/^#DNS=/DNS=192.168.11.2/g' \
        -e 's/^#Domains=/Domains=hiroom2.com/g'
$ sudo reboot
```

## nmcli + DHCP
```
$ INTERFACE=eno1
$ BRIDGE=br0
$ sudo nmcli con add type bridge ifname ${BRIDGE}
$ sudo nmcli con mod bridge-${BRIDGE} bridge.stp no
$ sudo nmcli con add type bridge-slave ifname ${INTERFACE} master bridge-${BRIDGE}
$ sudo reboot
```

## nmcli + Static IP Address
```
$ sudo nmcli con add type bridge ifname br0
$ sudo nmcli con mod bridge-br0 bridge.stp no
$ sudo nmcli con add type bridge-slave ifname ${INTERFACE} master bridge-br0
$ sudo nmcli con modify bridge-br0 ipv4.method manual \
       ipv4.address "192.168.11.250/24" \
       ipv4.gateway "192.168.11.1" \
       ipv4.dns "192.168.11.2" \
       ipv4.dns-search "hiroom2.com"
$ sudo reboot
```

## systemd-networkd + DHCP
```
$ INTERFACE=ens3
$ MACADDR=$(ip a s ${INTERFACE} | grep 'link/ether' | awk '{ print $2 }')
$ cat <<EOF | sudo tee /etc/systemd/network/br0.netdev
[NetDev]
Name=br0
Kind=bridge
EOF
$ cat <<EOF | sudo tee /etc/systemd/network/br0.network
[Match]
Name=br0

[Link]
MACAddress=${MACADDR}

[Network]
DHCP=yes
EOF
$ cat <<EOF | sudo tee /etc/systemd/network/${INTERFACE}.network
[Match]
Name=${INTERFACE}

[Network]
Bridge=br0

[Network]
DHCP=no
EOF
$ sudo systemctl enable systemd-networkd
$ sudo reboot
```

## systemd-networkd + Static IP Address
```
$ INTERFACE=ens3
$ MACADDR=$(ip a s ${INTERFACE} | grep 'link/ether' | awk '{ print $2 }')
$ cat <<EOF | sudo tee /etc/systemd/network/br0.netdev
[NetDev]
Name=br0
Kind=bridge
EOF
$ cat <<EOF | sudo tee /etc/systemd/network/br0.network
[Match]
Name=br0

[Link]
MACAddress=${MACADDR}

[Network]
Address=192.168.11.250/24
Gateway=192.168.11.1
DNS=192.168.11.2
Domain=hiroom2.com
EOF
$ cat <<EOF | sudo tee /etc/systemd/network/${INTERFACE}.network
[Match]
Name=${INTERFACE}

[Network]
Bridge=br0

[Network]
DHCP=no
EOF
$ sudo systemctl enable systemd-networkd
$ sudo reboot
```

## QEMU Guide to Bridged Networking

https://github.com/foxlet/macOS-Simple-KVM/blob/master/docs/guide-passthrough.md

## Bridge with iproute2

    ## https://wiki.archlinux.org/title/Network_bridge
    $ sudo ip link add name bridge_name type bridge
    $ sudo ip link set bridge_name up
    $ sudo ip link set eth0 up
    $ sudo ip link set eth0 master bridge_name
    $ sudo bridge link

    ## delete brdige
    $ sudo ip link set eth0 nomaster
    $ sudo ip link set eth0 down
    $ ip link delete bridge_name type bridge

## Bridge with netplan

    ## /etc/netplan/01-netcfg.yaml (or whatever file you have there, could also be 50-cloud-init.yaml)
    $ cat /etc/netplan/00-installer-config.yaml
    network:
      version: 2
      renderer: networkd
      ethernets:
        eno1:
          dhcp4: no
      bridges:
        br0:
          interfaces: [eno1]
          dhcp4: yes
    $ netplan apply

## Configuring a network bridge using nmcli commands

    ## Step 1: Create a bridge interface
    $ nmcli connection add type bridge con-name bridge0 ifname bridge0

    ## Step 2: Display the network interfaces, and note the names of the interfaces you want to add to the bridge
    $ nmcli device status
    DEVICE  TYPE      STATE         CONNECTION
    enp7s0  ethernet  disconnected  --
    enp8s0  ethernet  disconnected  --
    bond0   bond      connected     bond0
    bond1   bond      connected     bond1

    ## Step 3: Assign the interfaces to the bridge
    ## Create new connection profiles for them
    $ nmcli connection add type ethernet slave-type bridge con-name bridge0-port1 ifname enp7s0 master bridge0
    $ nmcli connection add type ethernet slave-type bridge con-name bridge0-port2 ifname enp8s0 master bridge0
    ## Assign an existing connection profile
    $ nmcli connection modify bond0 master bridge0
    $ nmcli connection modify bond1 master bridge0

    ## Step 4: Configure the IP settings of the bridge. Skip this step if you want to use this bridge as a ports of other devices.
    $ nmcli connection modify bridge0 ipv4.addresses '192.0.2.1/24'
    $ nmcli connection modify bridge0 ipv4.gateway '192.0.2.254'
    $ nmcli connection modify bridge0 ipv4.dns '192.0.2.253'
    $ nmcli connection modify bridge0 ipv4.dns-search 'example.com'
    $ nmcli connection modify bridge0 ipv4.method manual

    $ nmcli connection modify bridge0 ipv6.addresses '2001:db8:1::1/64'
    $ nmcli connection modify bridge0 ipv6.gateway '2001:db8:1::fffe'
    $ nmcli connection modify bridge0 ipv6.dns '2001:db8:1::fffd'
    $ nmcli connection modify bridge0 ipv6.dns-search 'example.com'
    $ nmcli connection modify bridge0 ipv6.method manual

    ## Step 5: Optional: Configure further properties of the bridge, By default, STP is enabled.
    $ nmcli connection modify bridge0 bridge.priority '16384'

    ## Step 6: Activate the connection
    $ nmcli connection up bridge0

    ## Step 7: Verify that the ports are connected
    $ nmcli device
    DEVICE   TYPE      STATE      CONNECTION
    ...
    enp7s0   ethernet  connected  bridge0-port1
    enp8s0   ethernet  connected  bridge0-port2

    ## Enable the connection.autoconnect-slaves
    $ nmcli connection modify bridge0 connection.autoconnect-slaves 1
    $ nmcli connection up bridge0

    $ ip link show master bridge0
    3: enp7s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc fq_codel master bridge0 state UP mode DEFAULT group default qlen 1000
    link/ether 52:54:00:62:61:0e brd ff:ff:ff:ff:ff:ff
    4: enp8s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc fq_codel master bridge0 state UP mode DEFAULT group default qlen 1000
        link/ether 52:54:00:9e:f1:ce brd ff:ff:ff:ff:ff:ff
    $ bridge link show
    3: enp7s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 master bridge0 state forwarding priority 32 cost 100
    4: enp8s0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 master bridge0 state listening priority 32 cost 100
