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

3. Could not get lock /var/lib/dpkg/lock

        ## Find and Kill all apt-get or apt Processes
        $ ps -A | grep apt
        $ sudo kill -9 13431

        ## Delete the lock Files
        $ sudo rm /var/lib/dpkg/lock
        $ sudo dpkg --configure -a

        $ sudo rm /var/lib/apt/lists/lock
        $ sudo rm /var/cache/apt/archives/lock
        $ sudo apt update
        OR
        $ sudo apt-get update

3. Maintain enter ro single

        - press e grub menu
        - change linuz command line add to 'ro single'
            root=/dev/mapper/olddebian-root ro single
        - enter root passwd enter single mode

4. Install and uninstall xfce4

        $ sudo apt install xfce4
        $ sudo apt remove xfce4
        $ sudo apt remove xfce4*
        $ sudo apt purge xfconf xfce4-utils xfwm4 xfce4-session thunar xfdesktop4 exo-utils xfce4-panel xfce4-terminal libxfce4util-common scim xscreensaver
        $ sudo apt autoremove

5. Fix “system program problem detected” error in Ubuntu

        $ sudo rm /var/crash/*
        $ sudo vim /etc/default/apport
          enable=0
