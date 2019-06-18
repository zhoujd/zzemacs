firewall
========

## Ubuntu ufw

    $ sudo /etc/init.d/ufw restart
    $ sudo ufw enable
    $ sudo ufw disable
    
    $ sudo ufw status
    $ sudo ufw status numbered
    
    $ sudo ufw app list
    $ sudo ufw app info OpenSSH
    
    $ sudo ufw allow 8810/tcp
    $ sudo ufw allow ssh
