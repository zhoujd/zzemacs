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
