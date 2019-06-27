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
         
