sshfs
=====

1. install sshfs

        $ sudo apt-get install sshfs

2. mount with sshfs

        $ sudo mkdir /mnt/droplet
        $ sudo sshfs -o allow_other,defer_permissions root@xxx.xxx.xxx.xxx:/ /mnt/droplet
        $ sudo sshfs -o allow_other,defer_permissions,IdentityFile=~/.ssh/id_rsa root@xxx.xxx.xxx.xxx:/ /mnt/droplet

3. umount ssfs

        $ sudo umount /mnt/droplet
        $ sudo fusermount -u /mnt/droplet

4. Permanently Mounting the Remote File System

        $ sudo nano /etc/fstab
          sshfs#root@xxx.xxx.xxx.xxx:/ /mnt/droplet

