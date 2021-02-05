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

