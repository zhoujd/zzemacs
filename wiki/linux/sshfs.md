sshfs
=====

## install sshfs

    $ sudo apt-get install sshfs

## mount with sshfs

    $ sudo mkdir /mnt/droplet
    $ sudo sshfs -o allow_other,defer_permissions root@xxx.xxx.xxx.xxx:/ /mnt/droplet
    $ sudo sshfs -o allow_other,defer_permissions,IdentityFile=~/.ssh/id_rsa root@xxx.xxx.xxx.xxx:/ /mnt/droplet

## umount ssfs

    $ sudo umount /mnt/droplet
    $ sudo fusermount -u /mnt/droplet

## Permanently Mounting the Remote File System

    $ sudo nano /etc/fstab
    sshfs#root@xxx.xxx.xxx.xxx:/ /mnt/droplet
