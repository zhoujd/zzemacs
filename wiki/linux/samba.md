Samba Server
============

1. Installing Samba on Ubuntu

        $ sudo apt update
        $ sudo apt install samba
        $ sudo apt install smbclient

2. Setting up Samba

        $ mkdir /home/<username>/sambashare/
        $ sudo cp /etc/samba/smb.conf{,.backup}
        $ sudo nano /etc/samba/smb.conf
         ## At the bottom of the file, add the following lines:
         [sambashare]
             comment = Samba on Ubuntu
             path = /home/username/sambashare
             read only = no
             browsable = yes
             valid users = jiandon

        $ sudo smbpasswd -a jiandon
        $ smbclient -L localhost

        $ sudo ufw allow 'Samba'
        $ sudo service smbd restart

3. Setting up User Accounts and Connecting to Share

        $ sudo smbpasswd -a username
        $ sudo smbpasswd -e username

4. Using the smbclient client

        $ sudo apt install smbclient
        $ sudo yum install samba-client

        $ smbclient //samba_hostname_or_server_ip/share_name -U username
        $ smbclient //192.168.121.118/josh -U josh


4. Mounting the Samba share

        $ sudo apt install cifs-utils
        $ sudo yum install cifs-utils

        ## Note: file_mode=0777,dir_mode=0777
        $ sudo mount -t cifs -o username=username,password=password,noperm //samba_hostname_or_server_ip/sharename /mnt/smbmount
        $ sudo mount -t cifs -o username=zach //192.168.121.118/sharename /mnt/smbmount

5. Mount Samba share using fstab

        $ cat /ect/fstab
        //192.168.122.52/user1  /mnt/shares cifs credentials=/.sambacreds 0 0

        $ cat /.sambacreds
        username=user1
        password=password
        domain=WORKGROUP
