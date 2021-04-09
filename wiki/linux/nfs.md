NFS
===

1. Install NFS Server on CentOS

        # yum -y install --disablerepo=* --enablerepo=localbase nfs-utils

        # cat /etc/exports
        /home/lmsdk/vca-share              *(rw,sync,no_root_squash,no_all_squash)

        # systemctl stop nfs-server
        # systemctl disable nfs-server
        # systemctl enable rpcbind
        # systemctl enable nfs-server
        # systemctl start rpcbind
        # systemctl start nfs-server

        # yum -y install firewalld
        # systemctl start firewalld.service
        # systemctl enable firewalld.service
        # firewall-cmd --permanent --zone=public --add-service=ssh
        # firewall-cmd --permanent --zone=public --add-service=nfs
        # firewall-cmd --reload

        # mkdir /var/nfs
        # chown nfsnobody:nfsnobody /var/nfs
        # chmod 755 /var/nfs
        # cat /etc/exports
         /home           192.168.1.101(rw,sync,no_root_squash,no_subtree_check)
         /var/nfs        192.168.1.101(rw,sync,no_subtree_check)
        # exportfs -a

2. Install NFS Server on Ubuntu

        $ sudo apt install nfs-kernel-server
        # cat /etc/exports
        /home/username/share		192.168.1.0/24(rw,sync,no_subtree_check)

        $ sudo systemctl restart nfs-kernel-server

3. Install NFS Client on Ubuntu

        $ sudo apt install nfs-common

4. Mount NFS on Client

        $ sudo mount 192.168.1.110:/home/username/share /mnt/nfs

        ## mount at startup
        $ cat /etc/fstab
        192.168.1.110:/home/username/share		/mnt/nfs	nfs defaults,user,exec		0 0

