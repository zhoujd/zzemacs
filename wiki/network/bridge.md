Bridge
======

https://www.hiroom2.com/2018/05/08/ubuntu-1804-bridge-en/
    
1. bridge-utils + DHCP
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

2. bridge-utils + Static IP Address
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

3. nmcli + DHCP
```
$ sudo nmcli con add type bridge ifname br0
$ sudo nmcli con mod bridge-br0 bridge.stp no
$ sudo nmcli con add type bridge-slave ifname ${INTERFACE} master bridge-br0
$ sudo reboot
```

4. nmcli + Static IP Address
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

5. systemd-networkd + DHCP
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

6. systemd-networkd + Static IP Address
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
