Ubuntu setting
==============

1. English date etc.
   
        System settings -> Language support -> Regional format
        => English(United States)
        Reboot

2. Firewall

        telnet 192.168.1.103 80
        sudo ufw status
        sudo ufw allow 80
        sudo ufw enable
        sudo ufw reload

