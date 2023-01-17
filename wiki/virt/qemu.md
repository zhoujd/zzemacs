QEMU
====

## Install QEMU on Ubuntu

    $ sudo apt install qemu qemu-utils qemu-kvm virt-manager libvirt-daemon-system libvirt-clients bridge-utils
    $ sudo usermod -aG kvm,libvirt,libvirt-qemu $USER

## Install QEMU on CentOS

    [QEMU for windows]<http://qemu.weilnetz.de/>
    [QEMU wiki]<http://wiki.qemu.org/Main_Page>

    $ sudo apt-get install kvm qemu qemu-kvm virt-manager kernel-package linux-source kqemu-source build-essential
    $ sudo yum install kvm kmod-kvm qemu
    $ modprobe kvm-intel or modprobe kvm-amd
    $ /sbin/lsmod | grep kvm
    $ sudo yum provides "*/qemu-kvm"
    $ sudo qemu-img create –f qcow windows.img 8G
    $ sudo kvm -localtime -cdrom /dev/cdrom -m 512 -boot d win2.img
    $ sudo kvm -localtime -m 512 -hda windows.img -cdrom winxp.iso -boot d -clock -rtc -no-acpi

## Creating Virtual machines

    $ qemu-img create ubuntu.img 20G
    $ qemu-img create -f qcow2 ubuntu.qcow 20G
    $ qemu-system-x86_64 -hda ubuntu.img -boot d -cdrom ubuntu-20.04-server-amd64.iso -m 640
    $ qemu-system-i386 -hda ubuntu.img -boot d -cdrom ubuntu-20.04-server-i386.iso -m 640
    $ qemu -hda ubuntu.img -m 640

## URLs

    https://wiki.qemu.org/Documentation/Networking
    https://wiki.qemu.org/Documentation/Networking/NAT
    https://wiki.gentoo.org/wiki/QEMU/Bridge_with_Wifi_Routing

## The fun of routing to Wifi

    $ sudo cat  /etc/sysctl.conf
      net.ipv4.ip_forward = 1
    $ sudo sysctl -p /etc/sysctl.conf

    ## Now, in this example, the bridge interface is br0 and the host system is connected through wlan0.
    ## First, help the traffic get through the wlan0
    $ sudo iptables -A FORWARD -i br0 -o wlan0 -j ACCEPT
    $ sudo iptables -A POSTROUTING -t nat -o wlan0 -j MASQUERADE

    ## Then, let the system know that the known traffic can get back at br0:
    # sudo iptables -A FORWARD -i wlan0 -o br0 -m state --state RELATED,ESTABLISHED -j ACCEPT

## Expanding the Filesystem

    ## The qcow2 image is built such that, by running a simple command from within the VM environment,
    ## you can resize the root filesystem to fill the partitioned space.
    ## Note
    ## In this example, the filesystem to be resized is /dev/vda2 but it will depend on the drives that are available in the VM image
    ## It could be /dev/sda2 or something similar.
    ## Before the rootfs is resized, running df -h from within the VM environment will show you that /dev/vda2 has a size of 3.1G.
    ## To grow the partition, resize the /dev/vda2 partition and then check df -h again to see that it is now 31G:

    purism@pureos:~$ sudo resize2fs /dev/vda2
    resize2fs 1.44.4 (18-Aug-2018)
    Filesystem at /dev/vda2 is mounted on /; on-line resizing required
    old_desc_blocks = 1, new_desc_blocks = 4
    The filesystem on /dev/vda2 is now 8172342 (4k) blocks long.

    purism@pureos:~$ df -h /
    Filesystem      Size  Used Avail Use% Mounted on
    /dev/vda2        31G  2.2G   28G   8% /
    Now your additional space on /dev/vda2 is immediately ready for use without any need to reboot.

## Qemu Guest Agent Integration

    ## http://wiki.stoney-cloud.org/wiki/Qemu_Guest_Agent_Integration
    ## https://wiki.libvirt.org/page/Qemu_guest_agent

## Setting the TAP network for QEMU

    ## https://xilinx-wiki.atlassian.net/wiki/spaces/A/pages/862912682/Networking+in+QEMU
    $ sudo apt-get install bridge-utils
    $ sudo apt-get install uml-utilities

    ## Create a bridge named br0
    $ brctl addbr br0

    ## Add eth0 interface to bridge
    $ brctl addif br0 eth0

    ## Create tap interface.
    $ tunctl -t tap0 -u `whoami`

    ## Add tap0 interface to bridge.
    $ brctl addif br0 tap0

    ## Check/Bring up all interfaces.
    $ ifconfig eth0 up
    $ ifconfig tap0 up
    $ ifconfig br0 up

    ## Check if bridge is set properly.
    $ brctl show

    ## Assign IP address to bridge 'br0'.
    $ dhclient -v br0

    ## Boot QEMU
    $ petalinux-boot --qemu --kernel --qemu-args="-net nic -net nic -net nic -net nic -net tap,ifname=tap0,script=no,downscript=no"
    $ petalinux-boot --qemu --kernel --qemu-args="-net nic -net nic -net tap,ifname=tap0,script=no,downscript=no"

## Check if the virtual machine is already in required state or not

    #!/bin/bash
    tmp=$(virsh list --all | grep " vmtest " | awk '{ print $3}')
    if ([ "x$tmp" == "x" ] || [ "x$tmp" != "xrunning" ])
    then
        echo "VM does not exist or is shut down!"
        # Try additional commands here...
    else
        echo "VM is running!"
    fi

    #!/bin/bash
    virsh domstate vmtest | grep running
    if [ $? -ne 0 ] ; then
        echo Starting VM vmtest
        virsh start vmtest
    fi

## Modify (extend) the LVM

    ## Extend guest VM disk
    $ sudo qemu-img resize /var/lib/libvirt/images/rhel8.qcow2 +10G
    $ sudo qemu-img info /var/lib/libvirt/images/rhel8.qcow2

    ## Tell LVM the physical partition size has changed:
    $ lsblk
    $ sudo pvresize /dev/sda3

    ## Find the actual path of the LVM logical volume:
    $ sudo lvdisplay  # The LV Path is the value needed

    ## Tell LVM to extend the logical volume to use all of the new partition size:
    $ sudo lvextend -l +100%FREE /dev/name-of-volume-group/root  # Using the LV Path from above

    ## Resize the file system:
    $ sudo resize2fs /dev/name-of-volume-group/root

## KVM UEFI boot for Virtual Machine

    ## Install UEFI Firmware for Virtual Machines
    $ sudo apt install ovmf

    ## Specify [--boot uefi] When creating Virtual Machine
    $ sudo virt-install \
        --name Win2k22 \
        --ram 6144 \
        --disk path=/var/kvm/images/Win2k22.img,size=100 \
        --vcpus 4 \
        --os-variant win2k22 \
        --network bridge=br0 \
        --graphics vnc,listen=0.0.0.0,password=password \
        --video vga \
        --cdrom /home/Win2022_EN-US_20348.169.210806-2348.fe.iso \
        --boot uefi

    ## Virtual Machine starts on UEFI mode
