virt-manager
============

## How To Backup A Virt-Manager Virtual Machine On Linux

    ## https://www.addictivetips.com/ubuntu-linux-tips/backup-a-virt-manager-virtual-machine-on-linux/
    $ cd /var/lib/libvirt/images/
    $ sudo -s
    $ echo '' > nameofvm.xml
    $ virsh dumpxml nameofvm >  /var/lib/libvirt/images/nameofvm.xml
    $ mkdir -p vm-backup
    $ mv example.qcow2 vm-backup
    $ mv nameofvm.xml vm-backup
    $ tar -jcvfp my-vm-backup.tar.bz2 vm-backup
    $ mv my-vm-backup.tar.bz2 /home/username/

    ## restore vm
    $ tar -xvfp my-vm-backup.tar.bz2
    $ cd vm-backup
    $ sudo -s
    $ virsh define --file nameofvm.xml
    $ mv example.qcow2 /var/lib/libvirt/images/

## Virsh commands cheatsheet

    $ virsh nodeinfo
    $ virsh list --all
    $ virsh list
    $ virsh domrename currentname  newname
    $ virsh edit domain
    $ virsh start test
    $ virsh autostart test
    $ virsh autostart --disable test
    $ virsh dominfo test
    $ virsh shutdown test
    $ virsh destroy test
    $ for i in `sudo virsh list | grep running | awk '{print $2}'` do
        sudo virsh shutdown $i
      done
    $ virsh reboot test
    $ virsh console test
    $ virsh console test --force

## How to fix "network 'default' is not active" error in libvirt

    ## https://libvirt.org/sources/virshcmdref/html/sect-net-autostart.html
    ## https://www.xmodulo.com/network-default-is-not-active.html
    $ sudo virsh net-list --all
    $ sudo virsh net-start default
    $ sudo virsh net-autostart --network default --disable
