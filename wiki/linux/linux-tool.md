Linux Tool
==========

## Linuxkit 

    git clone https://github.com/linuxkit/linuxkit
    cd linuxkit && make
    cp -rf bin/linuxkit /usr/local/linuxkit
    linuxkit build -format iso-bios -name kitiso minimal.yml
    du -sh * | grep linuxkit.iso 
    file linuxkit.iso

## chage command in Linux

    ## chage command is used to view and change the user password expiry information
    $ USERADDCMD="useradd -m <user>"
    $ USERCHGCMD="chage -M 99999 sys_cert;passwd -d <user>"
    $ getent passwd 2>/dev/null | grep -m 1 -e <user>
    $ hostnamectl status
