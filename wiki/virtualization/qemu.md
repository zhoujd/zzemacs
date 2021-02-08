QEMU
====

1. Install QEMU in Ubuntu 20.04

        $ sudo apt install qemu qemu-utils qemu-kvm virt-manager libvirt-daemon-system libvirt-clients bridge-utils
        $ sudo usermod -aG kvm,libvirt,dnsmasq,libvirt-qemu $USER
        
2. Creating Virtual machines

        $ qemu-img create ubuntu.img 20G
        $ qemu-img create -f qcow2 ubuntu.qcow 20G
        $ qemu-system-x86_64 -hda ubuntu.img -boot d -cdrom /home/sk/Soft_Backup/OS\ Images/New/ubuntu-15.04-server-amd64.iso -m 640
        $ qemu-system-xi386 -hda ubuntu.img -boot d -cdrom /home/sk/Soft_Backup/OS\ Images/New/ubuntu-15.04-server-i386.iso -m 640
        $ qemu -hda ubuntu.img -m 640

3. URLs

    https://wiki.qemu.org/Documentation/Networking
    https://wiki.qemu.org/Documentation/Networking/NAT
    https://wiki.gentoo.org/wiki/QEMU/Bridge_with_Wifi_Routing

4. The fun of routing to Wifi

        # cat  /etc/sysctl.conf
          net.ipv4.ip_forward = 1
        # sysctl -p /etc/sysctl.conf
        
        ##Now, in this example, the bridge interface is br0 and the host system is connected through wlan0. 
        ##First, help the traffic get through the wlan0
        # iptables -A FORWARD -i br0 -o wlan0 -j ACCEPT
        # iptables -t nat -A POSTROUTING -o wlan0 -j MASQUERADE
         
        ##Then, let the system know that the known traffic can get back at br0:
        # iptables -A FORWARD -i wlan0 -o br0 -m state --state RELATED,ESTABLISHED -j ACCEPT

5. Expanding the Filesystem
    
        ##The qcow2 image is built such that, by running a simple command from within the VM environment, 
        ##you can resize the root filesystem to fill the partitioned space.
        ##Note
        ##In this example, the filesystem to be resized is /dev/vda2 but it will depend on the drives that are available in the VM image 
        ## – it could be /dev/sda2 or something similar.
        ##Before the rootfs is resized, running df -h from within the VM environment will show you that /dev/vda2 has a size of 3.1G. 
        ##To grow the partition, resize the /dev/vda2 partition and then check df -h again to see that it is now 31G:

        purism@pureos:~$ sudo resize2fs /dev/vda2
        resize2fs 1.44.4 (18-Aug-2018)
        Filesystem at /dev/vda2 is mounted on /; on-line resizing required
        old_desc_blocks = 1, new_desc_blocks = 4
        The filesystem on /dev/vda2 is now 8172342 (4k) blocks long.

        purism@pureos:~$ df -h /
        Filesystem      Size  Used Avail Use% Mounted on
        /dev/vda2        31G  2.2G   28G   8% /
        Now your additional space on /dev/vda2 is immediately ready for use without any need to reboot.

6. Qemu Guest Agent Integration

        ##http://wiki.stoney-cloud.org/wiki/Qemu_Guest_Agent_Integration
        ##https://wiki.libvirt.org/page/Qemu_guest_agent

