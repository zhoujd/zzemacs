LIBVIRT
=======

## Disable libvirt networking

    ## https://docs.openstack.org/newton/networking-guide/misc-libvirt.html
    ## Linux bridging for implementing a layer 2 network
    ## dnsmasq for providing IP addresses to virtual machines using DHCP
    ## iptables to implement SNAT so instances can connect out to the public internet,
    ## and to ensure that virtual machines are permitted to communicate with dnsmasq using DHCP

    ## By default, libvirt creates a network named default
    ## a Linux bridge named virbr0 with an IP address of 192.168.122.1/24
    ## a dnsmasq process that listens on the virbr0 interface and hands out IP addresses in the range 192.168.122.2-192.168.122.254
    ## a set of iptables rules

    ## On Ubuntu, the iptables ruleset that libvirt creates includes the following rules
    *nat
    -A POSTROUTING -s 192.168.122.0/24 -d 224.0.0.0/24 -j RETURN
    -A POSTROUTING -s 192.168.122.0/24 -d 255.255.255.255/32 -j RETURN
    -A POSTROUTING -s 192.168.122.0/24 ! -d 192.168.122.0/24 -p tcp -j MASQUERADE --to-ports 1024-65535
    -A POSTROUTING -s 192.168.122.0/24 ! -d 192.168.122.0/24 -p udp -j MASQUERADE --to-ports 1024-65535
    -A POSTROUTING -s 192.168.122.0/24 ! -d 192.168.122.0/24 -j MASQUERADE
    *mangle
    -A POSTROUTING -o virbr0 -p udp -m udp --dport 68 -j CHECKSUM --checksum-fill
    *filter
    -A INPUT -i virbr0 -p udp -m udp --dport 53 -j ACCEPT
    -A INPUT -i virbr0 -p tcp -m tcp --dport 53 -j ACCEPT
    -A INPUT -i virbr0 -p udp -m udp --dport 67 -j ACCEPT
    -A INPUT -i virbr0 -p tcp -m tcp --dport 67 -j ACCEPT
    -A FORWARD -d 192.168.122.0/24 -o virbr0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    -A FORWARD -s 192.168.122.0/24 -i virbr0 -j ACCEPT
    -A FORWARD -i virbr0 -o virbr0 -j ACCEPT
    -A FORWARD -o virbr0 -j REJECT --reject-with icmp-port-unreachable
    -A FORWARD -i virbr0 -j REJECT --reject-with icmp-port-unreachable
    -A OUTPUT -o virbr0 -p udp -m udp --dport 68 -j ACCEPT

    ## dnsmasq process
    $ /usr/sbin/dnsmasq --conf-file=/var/lib/libvirt/dnsmasq/default.conf

    ## How to disable libvirt networks
    $ virsh net-list
    $ virsh net-destroy default
    $ virsh net-autostart --network default --disable
    $ virsh net-start default

## Automagically iptables rules added by libvirt

    $ cat /etc/libvirt/hooks/qemu
    #!/bin/bash

    if [ "$2" = "prepare" ]; then
      /sbin/iptables -D INPUT -i virbr0 -p udp --dport 53 -j ACCEPT
      /sbin/iptables -D INPUT -i virbr0 -p tcp --dport 53 -j ACCEPT
      /sbin/iptables -D INPUT -i virbr0 -p udp --dport 67 -j ACCEPT
      /sbin/iptables -D INPUT -i virbr0 -p tcp --dport 67 -j ACCEPT
      /sbin/iptables -D FORWARD -o virbr0 -d 192.168.0.32/28 -m state --state RELATED,ESTABLISHED -j ACCEPT
      /sbin/iptables -D FORWARD -i virbr0 -s 192.168.0.32/28 -j ACCEPT
      /sbin/iptables -D FORWARD -i virbr0 -o virbr0 -j ACCEPT
      /sbin/iptables -D FORWARD -o virbr0 -j REJECT
      /sbin/iptables -D FORWARD -i virbr0 -j REJECT
      /sbin/iptables -t nat -D POSTROUTING -s 192.168.0.32/28 ! -d 192.168.0.32/28 -p tcp -j MASQUERADE --to-ports 1024-65535
      /sbin/iptables -t nat -D POSTROUTING -s 192.168.0.32/28 ! -d 192.168.0.32/28 -p udp -j MASQUERADE --to-ports 1024-65535
      /sbin/iptables -t nat -D POSTROUTING -s 192.168.0.32/28 ! -d 192.168.0.32/28 -j MASQUERADE
    fi

## libvirt in iptables

    $ sudo iptables-save | grep -i libvirt
    :LIBVIRT_PRT - [0:0]
    -A POSTROUTING -j LIBVIRT_PRT
    -A LIBVIRT_PRT -o virbr0 -p udp -m udp --dport 68 -j CHECKSUM --checksum-fill
    :LIBVIRT_FWI - [0:0]
    :LIBVIRT_FWO - [0:0]
    :LIBVIRT_FWX - [0:0]
    :LIBVIRT_INP - [0:0]
    :LIBVIRT_OUT - [0:0]
    -A INPUT -j LIBVIRT_INP
    -A FORWARD -j LIBVIRT_FWX
    -A FORWARD -j LIBVIRT_FWI
    -A FORWARD -j LIBVIRT_FWO
    -A OUTPUT -j LIBVIRT_OUT
    -A LIBVIRT_FWI -d 192.168.122.0/24 -o virbr0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    -A LIBVIRT_FWI -o virbr0 -j REJECT --reject-with icmp-port-unreachable
    -A LIBVIRT_FWO -s 192.168.122.0/24 -i virbr0 -j ACCEPT
    -A LIBVIRT_FWO -i virbr0 -j REJECT --reject-with icmp-port-unreachable
    -A LIBVIRT_FWX -i virbr0 -o virbr0 -j ACCEPT
    -A LIBVIRT_INP -i virbr0 -p udp -m udp --dport 53 -j ACCEPT
    -A LIBVIRT_INP -i virbr0 -p tcp -m tcp --dport 53 -j ACCEPT
    -A LIBVIRT_INP -i virbr0 -p udp -m udp --dport 67 -j ACCEPT
    -A LIBVIRT_INP -i virbr0 -p tcp -m tcp --dport 67 -j ACCEPT
    -A LIBVIRT_OUT -o virbr0 -p udp -m udp --dport 53 -j ACCEPT
    -A LIBVIRT_OUT -o virbr0 -p tcp -m tcp --dport 53 -j ACCEPT
    -A LIBVIRT_OUT -o virbr0 -p udp -m udp --dport 68 -j ACCEPT
    -A LIBVIRT_OUT -o virbr0 -p tcp -m tcp --dport 68 -j ACCEPT
    :LIBVIRT_PRT - [0:0]
    -A POSTROUTING -j LIBVIRT_PRT
    -A LIBVIRT_PRT -s 192.168.122.0/24 -d 224.0.0.0/24 -j RETURN
    -A LIBVIRT_PRT -s 192.168.122.0/24 -d 255.255.255.255/32 -j RETURN
    -A LIBVIRT_PRT -s 192.168.122.0/24 ! -d 192.168.122.0/24 -p tcp -j MASQUERADE --to-ports 1024-65535
    -A LIBVIRT_PRT -s 192.168.122.0/24 ! -d 192.168.122.0/24 -p udp -j MASQUERADE --to-ports 1024-65535
    -A LIBVIRT_PRT -s 192.168.122.0/24 ! -d 192.168.122.0/24 -j MASQUERADE

## Build libvirt dependence on Fedora

    ## Prerequisites
    $ sudo dnf install -y automake gcc git make glibc glibc-utils glib2-devel zlib-devel pixman-devel flex bison
    $ sudo dnf install -y numad numactl-devel numactl-libs numactl
    $ sudo dnf install -y libtool gnutls-utils gnutls-devel libnl3-devel libxml2-devel
    $ sudo dnf install -y libtirpc-devel python3-docutils device-mapper-devel libpciaccess-devel
    $ sudo dnf install -y rpcbind readline-devel rpcgen yajl-devel libxslt-devel bzip2
    $ pip uninstall rst2html5
    $ pip uninstall docutils
    $ pip install docutils

## Build libvirt dependence on Ubuntu

    ## https://ubuntu.com/server/docs/libvirt
    ## https://libvirt.org/downloads.html

    ## Dependence
    $ sudo apt install libxml2-utils xsltproc pkg-config libfuse3-dev libfuse-dev
    $ sudo apt install libglib2.0-dev libglusterfs-dev libgnutls28-dev libiscsi-dev
    $ sudo apt install libnbd-dev libnl-3-dev libnl-route-3-dev libparted-dev
    $ sudo apt install libpcap-dev libssh-dev libssh2-1-dev libxml2-dev wireshark-dev
    $ sudo apt install libyajl-dev gettext libnuma-dev libopenwsman-dev libpciaccess-dev l
    $ sudo apt install ibrbd-dev librados-dev libreadline-dev libsasl2-dev
    $ pip uninstall rst2html5
    $ pip uninstall docutils
    $ pip install docutils

## Compiling upstream libvirt and qemu from scratch

    ## https://developer.ibm.com/tutorials/compiling-libvirt-and-qemu/
    ## Steps
    ## 1. Install Git and clone both upstream libvirt and qemu repos.
    $ git clone https://github.com/qemu/qemu.git
    $ git clone https://github.com/libvirt/libvirt.git

    ## 2. Configure and build the qemu
    $ cd qemu
    $ mkdir -p build
    $ cd build
    ## if you want the x86 target too you can add "x86-softmmu" in the target list
    $ ../configure --enable-trace-backend=simple --enable-debug --target-list=ppc64-softmmu --prefix=/usr/local
    $ make -j
    $ sudo make install

    ## 3. Configure and build libvirt
    $ cd $HOME
    $ mkdir -p libvirt_build
    $ cd libvirt
    $ mkdir build
    $ meson setup build --prefix=$HOME/libvirt_build
    $ ninja -C build
    $ sudo ninja -C build install

    ## 4. Qemu in the /usr/local/bin directory, the libvirt binary files are in $HOME/libvirt_build
    $ ls /usr/local/bin
    ivshmem-client  ivshmem-server  \
    qemu-edid  qemu-ga  qemu-img  qemu-io  qemu-nbd  \
    qemu-pr-helper  qemu-storage-daemon  qemu-system-ppc64

    ## 5. Start the libvirt daemon (libvirtd) from the $HOME/libvirt_build
    ## The configuration files $HOME/libvirt_build/etc/libvirt
    $ sudo ./run src/virtlockd & (must be running in the background)
    $ sudo ./run src/virtlogd & (must be running in the background)
    $ sudo ./run src/libvirtd (the libvirtd daemon I run in the foreground to see logs)

    ## 6. Use the virsh commands
    $ cd ~/libvirt_build
    $ sudo ./run tools/virsh destroy  apic_test
    Domain apic_test destroyed
    $ sudo ./run tools/virsh list --all
    Id   Name        State
    ----------------------------
    -    apic_test   shut off

## To produce a build that is compatible with normal OS vendor prefixes

    ## https://libvirt.org/compiling.html
    ## Be aware that by default the build is configured with a local prefix path
    $ meson setup build -Dsystem=true
    $ meson setup build -Dsystem=true -Ddriver_qemu=enabled
    $ ninja -C build
    $ sudo ninja -C build install

## Build virt-manager on Ubuntu

    ## Remove old version
    $ sudo apt-get remove virt-manager

    ## Install build depend
    $ sudo apt build-dep virt-manager
    $ sudo apt install python-libvirt libgtk-3-dev libvirt-glib-1.0
    $ sudo apt install gir1.2-gtk-vnc-2.0 gir1.2-spice-client-gtk-3.0 libosinfo-1.0
    $ sudo apt install python-ipaddr gir1.2-vte-2.90  python-libxml2 python-requests

    ## Download source and build
    $ wget https://virt-manager.org/download/sources/virt-manager/virt-manager-1.3.2.tar.gz
    $ tar -xf virt-manager-1.3.2.tar.gz
    $ cd virt-manager-1.3.2
    $ sudo python setup.py install
    $ virt-manager
