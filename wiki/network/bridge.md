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
