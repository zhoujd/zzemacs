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

    ## https://computingforgeeks.com/virsh-commands-cheatsheet/
    $ virsh nodeinfo
    $ virsh list --all
    $ virsh list
    $ virsh domrename currentname  newname
    $ virsh edit domain
    $ virsh start test
    $ virsh autostart test
    $ virsh autostart --disable test
    $ virsh dominfo test
    $ virsh domifaddr test
    $ virsh domiflist test
    $ virsh shutdown test
    $ virsh destroy test
    $ for i in `sudo virsh list | grep running | awk '{print $2}'` do
        sudo virsh shutdown $i
      done
    $ virsh reboot test

    ## remove vm
    $ virsh destroy test 2> /dev/null
    $ virsh undefine  test
    $ virsh pool-refresh default
    $ virsh vol-delete --pool default test.qcow2
    $ virsh undefine test --remove-all-storage

    ## create a vm
    $ virt-install \
        --name centos7 \
        --description "Test VM with CentOS 7" \
        --ram=1024 \
        --vcpus=2 \
        --os-type=Linux \
        --os-variant=rhel7 \
        --disk path=/var/lib/libvirt/images/centos7.qcow2,bus=virtio,size=10 \
        --graphics none \
        --location $HOME/iso/CentOS-7-x86_64-Everything-1611.iso \
        --network bridge:virbr0 \
        --console pty,target_type=serial -x 'console=ttyS0,115200n8 serial'
    $ virsh console test
    $ virsh console test --force

    ## edit vm xml file
    $ export EDITOR=vim
    $ virsh edit test

    $ virsh suspend test
    $ virsh resume test
    $ virsh save test test.saved
    $ virsh restore test.save

    ## create volume
    $ virsh vol-create-as default test_vol2.qcow2 2G
    $ du -sh /var/lib/libvirt/images/test_vol2.qcow2
    $ virsh vol-list --pool default
    $ virsh vol-list --pool images

    ## attach a volume to vm
    $ virsh attach-disk --domain test \
        --source /var/lib/libvirt/images/test_vol2.qcow2 \
        --persistent --target vdb

    ## detach a volume from vm
    $ virsh detach-disk --domain test --persistent --live --target vdb
    s ssh test
    $ lsblk --output NAME,SIZE,TYPE

## How to fix "network 'default' is not active" error in libvirt

    ## https://libvirt.org/sources/virshcmdref/html/sect-net-autostart.html
    ## https://www.xmodulo.com/network-default-is-not-active.html
    $ sudo virsh net-list --all
    $ sudo virsh net-start default
    $ sudo virsh net-autostart --network default --disable
