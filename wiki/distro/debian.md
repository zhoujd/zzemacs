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

## Xorg

```
$ sudo apt install xorg
$ starx
```

## LXDM

```
$ sudo apt install lxdm
$ sudo lxdm-config
```

## Docker

```
# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/debian/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/debian \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
sudo docker run hello-world
```
