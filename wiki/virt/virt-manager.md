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
