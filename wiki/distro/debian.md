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

## Networkctl unmanaged

```
## Create a new .network file
$ sudo tee /etc/systemd/network/enp3s0.network <<EOF
[Match]
Name=enp3s0

[Network]
DHCP=ipv4
LinkLocalAddressing=no
EOF

## Restart the systemd-networkd service
$ sudo systemctl restart systemd-networkd
$ sudo networkctl renew enp3s0
$ ip address show dev enp3s0
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
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] \
  https://download.docker.com/linux/debian \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io \
    docker-buildx-plugin docker-compose-plugin
sudo docker run hello-world
```

## How to Create a Simple Debian Package

```
Step 1: Prepare the Files

First, create a working directory and prepare your application files.
For example, let's create a script and a configuration file:

$ mkdir -p ~/myapp/{bin,etc}
$ echo -e '#!/bin/bash\necho "Hello, World!"' > ~/myapp/bin/myapp.sh
$ echo 'NAME=MyApp' > ~/myapp/etc/myapp.conf

Step 2: Create the Control File

Next, create a DEBIAN directory and a control file with package metadata:

$ mkdir ~/myapp/DEBIAN
$ cat <<EOF > ~/myapp/DEBIAN/control
Package: myapp
Version: 1.0-1
Section: utils
Priority: optional
Architecture: all
Maintainer: Your Name <you@example.com>
Description: My simple app
EOF

Step 3: Build the Package

Use the dpkg-deb command to build the package:

$ dpkg-deb --build ~/myapp

This will create a myapp.deb file in your working directory.

Step 4: Verify the Package

You can verify the contents of the package using the dpkg command:

$ dpkg -c myapp.deb

Step 5: Install and Test

Finally, install and test your package:

$ sudo dpkg -i myapp.deb
$ myapp.sh

This should output "Hello, World!" if everything is set up correctly
By following these steps, you can create a simple .deb package for your application

For more complex packages, additional steps like dependency
management and post-install scripts may be required.
```
