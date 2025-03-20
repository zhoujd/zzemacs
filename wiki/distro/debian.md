Debian
======

## Network ISC DHCP client (dhclient) alternative

```
## 1. Uninstall dhclient
$ sudo apt purge -y isc-dhcp-client
$ sudo apt purge -y isc-dhcp-common

## 2. Start systemd-networkd
$ sudo systemctl start systemd-networkd
$ sudo systemctl enable systemd-networkd

## 3. Make systemd-networkd manage network interfaces, using its own DHCP client
$ sudo tee /etc/systemd/network/05-enp1s0.network <<EOF
[Match]
Name=enp1s0

[Network]
DHCP=yes
EOF
$ sudo networkctl reload
$ sudo networkctl renew enp1s0
```

## Change the Timezone

```
$ timedatectl list-timezones
$ sudo timedatectl set-timezone Asia/Shanghai
```
