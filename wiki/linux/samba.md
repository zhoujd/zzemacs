Samba Server
============

## Installing Samba on Ubuntu

    $ sudo apt update
    $ sudo apt install samba
    $ sudo apt install smbclient
    $ sudo apt install cifs-utils

## Setting up Samba

    $ sudo mkdir -p /zach
    $ sudo chown zach:zach /zach
    $ sudo cp /etc/samba/smb.conf /etc/samba/smb.conf.origin
    $ sudo vim /etc/samba/smb.conf
    ## At the bottom of the file, add the following lines:
    [zach]
    comment = Data on Ubuntu
    path = /zach
    read only = no
    browsable = yes
    valid users = zach

   [home]
       comment = Home on Ubuntu
       path = /home/zach
       read only = no
       browsable = yes
       valid users = zach

    $ sudo smbpasswd -a zach
    $ smbclient -L localhost

    $ sudo ufw allow 'Samba'
    $ sudo service smbd restart

## Setting up User Accounts and Connecting to Share

    $ sudo smbpasswd -a username
    $ sudo smbpasswd -e username

## Using the smbclient client

    $ sudo apt install smbclient
    $ sudo yum install samba-client

    $ smbclient //samba_hostname_or_server_ip/share_name -U username
    $ smbclient //192.168.121.118/josh -U josh


## Mounting the Samba share

    $ sudo apt install cifs-utils
    $ sudo yum install cifs-utils

    ## Note: file_mode=0777,dir_mode=0777
    $ sudo mount -t cifs -o username=username,password=password,noperm //samba_hostname_or_server_ip/sharename /mnt/smbmount
    $ sudo mount -t cifs -o username=zach //192.168.121.118/sharename /mnt/smbmount

## Mount Samba share using fstab

    $ cat /ect/fstab
    //192.168.122.52/user1  /mnt/shares cifs credentials=/.sambacreds 0 0

    $ cat /.sambacreds
    username=user1
    password=password
    domain=WORKGROUP

## Mount CIFS Credentials File has Special Character

    ## https://serverfault.com/questions/309429/mount-cifs-credentials-file-has-special-character
    ## Solution is use credential file
    $ cat > cifs.credo
    username=myuser
    password=PASS,WORD
    domain=mydomain

    $ sudo mount -t cifs -o credentials=path/to/cifs.credo //server/share localfolder --verbose

## Mount to non-root user

    $ target=/data
    $ source=//jiandon-pc/data
    $ credo=/zach/script/cifs.credo
    $ sudo mount -t cifs -o credentials=$credo,noperm,uid=jiandon,gid=jiandon $source $target
