Ubuntu setting
==============

## English date etc.

    System settings -> Language support -> Regional format
    => English(United States)
    Reboot

## Firewall

    $ sudo apt install gufw

    $ telnet 192.168.1.103 80
    $ sudo ufw status
    $ sudo ufw allow 80
    $ sudo ufw enable
    $ sudo ufw reload

## Could not get lock /var/lib/dpkg/lock

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

## Maintain enter ro single

    - press e grub menu
    - change linuz command line add to 'ro single'
        root=/dev/mapper/olddebian-root ro single
    - enter root passwd enter single mode

## Install and uninstall xfce4

    $ sudo apt install xfce4
    $ sudo apt remove xfce4
    $ sudo apt remove xfce4*
    $ sudo apt purge xfconf xfce4-utils xfwm4 xfce4-session thunar xfdesktop4 exo-utils xfce4-panel xfce4-terminal libxfce4util-common scim xscreensaver
    $ sudo apt autoremove

## Fix "system program problem detected" error in Ubuntu

    $ sudo rm /var/crash/*
    $ sudo vim /etc/default/apport
      enable=0

## Server network configure

    $ sudo lshw -C network
    $ ethtool -h
    $ ethtool -s eth1 speed 1000 duplex full

## Setup xscreensaver

    $ sudo apt remove gnome-screensaver
    $ sudo apt install xscreensaver xscreensaver-gl-extra xscreensaver-data-extra

    $ mkdir -p ~/.config/systemd/user/
    $ nano ~/.config/systemd/user/xscreensaver.service
     [Unit]
     Description=XScreenSaver
     [Service]
     ExecStart=/usr/bin/xscreensaver -nosplash
     [Install]
     WantedBy=default.target
    $ systemctl --user enable xscreensaver

## Setup locale

    - Edit the locale file(s) in /var/lib/locales/supported.d/, and remove all unneeded locales (one locale per line)
    - Create and edit /etc/default/locale (see above for an example)
    - Delete all generated locale data: rm -rfv /usr/lib/locale/*
    - Re-generate new locales: locale-gen

    $ locale ... list the current locale configuration
    $ locale -a ... lists all all locales that were generated on your system
    $ locale -a -v ... list all locales and show useful additional information (such as directory names that contain the locale information data files)

    ## generate the missing locale and reconfigure locales
    $ sudo locale-gen "en_US.UTF-8"
    $ sudo dpkg-reconfigure locales

    ## removing locales and locale related files
    $ sudo apt-get install localepurge
    $ dpkg --configure localepurge
    $ sudo localepurge

## Change The Hostname On Ubuntu 18.04 LTS Server

    $ cat /etc/hostname
    $ cat /etc/hosts

    ## false -> true
    $ sudo nano /etc/cloud/cloud.cfg
    preserve_hostname: true

## Setup snap proxy

    $ sudo snap set core proxy.http=http://host:port/
    $ sudo snap set core proxy.https=http://host:port/
    $ sudo snap get core proxy
    $ sudo snap install foobar2000

## Install Microsoft fonts

    $ sudo apt install ttf-mscorefonts-installer

## Install microcode

    $ sudo apt install intel-microcode
    $ sudo apt install amd64-microcode

## Xfce auto run

    $ ls /etc/xdg/autostart/
    $ ls ~/.config/autostart/

## How To Delete A Repository And GPG Key In Ubuntu

    ## Delete A Repository In Ubuntu
    $ sudo nano /etc/apt/sources.list
    $ sudo add-apt-repository -r ppa:nemh/systemback
    $ sudo apt update

    ## Delete Repository keys
    $ sudo apt-key list
    $ sudo apt-key del "3820 03C2 C8B7 B4AB 813E 915B 14E4 9429 73C6 2A1B"
    $ sudo apt-key del 73C62A1B    # specify the last 8 characters only
    $ sudo apt update

## Modify login motd(Message of the Day)

    $ cd /etc/update-motd.d
    $ ls -l

    $ vim /etc/default/motd-news
      # Enable/disable the dynamic MOTD news service
      # This is a useful way to provide dynamic, informative
      # information pertinent to the users and administrators
      # of the local system
      ENABLED=0

    ## add landscape-sysinfo
    $ sudo apt install landscape-common
    $ landscape-sysinfo

## More package for Ubuntu

    $ sudo apt install ubuntu-restricted-extras
    ubuntu-restricted-addons

## Tools

    $ sudo apt install pinta
    $ sudo apt install redshift
    $ sudo apt install pdfchain

    ## https://gparted.org/download.php
    $ sudo apt install gparted

    ## camera & webcam
    $ sudo apt install v4l-utils
    $ v4l2-ctl --list-devices
      EasyCamera: EasyCamera (usb-0000:00:14.0-4):
           /dev/video0
           /dev/video1
    $ sudo apt install cheese

## Postsetting

    ## https://theoddblog.in/post-installation-setup-for-ubuntu/

## Install Google Chrome Browser

    $ wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
    $ sudo apt install ./google-chrome-stable_current_amd64.deb

## Thunar Thumbnail

    $ killall thunar
    $ sudo apt install tumbler tumbler-plugins-extra ffmpegthumbnailer
    $ thunar -q
    $ reboot

## Check package dependencies

    $ sudo apt install apt-rdepends
    ## apt-rdepends [options] [packages]
    ## find out the dependencies of php
    $ apt-rdepends php
    ## know what packages depend upon php
    $ apt-rdepends -r php

## eBook reader

    $ wget https://github.com/johnfactotum/foliate/releases/download/2.6.3/com.github.johnfactotum.foliate_2.6.3_all.deb

## Ubuntu git kernel repository

    ## https://kernel.ubuntu.com/git/ubuntu/

## Disable apt progress bar

    $ sudo apt -o Dpkg::Progress-Fancy="0" -o APT::Color="0" install vim-gtk

## Trash cli command line tool

    $ sudo apt install transh-cli
    $ trash
    $ trash-list
    $ trash-empty
    $ trash-put
    $ trash-rm
    $ trash-restore
