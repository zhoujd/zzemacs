Ubuntu setting
==============

## English date etc

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

## Screen keyboard

    ## GOK (Gnome screen keyboard)，kvkbd，onboard，Florence
    $ sudo apt install florence
    $ florence

    ## lightdm
    $ sudo apt install lightdm-gtk-greeter
    $ sudo vi /etc/lightdm/lightdm-gtk-greeter.conf
    [greeter]keyboard=florence --no-gnome --focus &

    ## autostart
    $ mkdir -p ~/.config/autostart
    $ vi ~/.config/autostart/florence.desktop
    [Desktop Entry]
    Type=Application
    Name=Virtual Keyboard
    Comment=Auto-start virtual keyboard
    Exec=florence --no-gnome

## Stopping an Ubuntu 22.04 desktop from suspending at the login screen

    ## https://utcc.utoronto.ca/~cks/space/blog/linux/Ubuntu2204DesktopStopSuspend
    ## sleep.conf, or more likely a drop in file /etc/systemd/sleep.conf.d/nope.conf
    $ cat /etc/systemd/sleep.conf.d/nope.conf
    # Never sleep or hibernate
    [Sleep]
    AllowSuspend=no
    AllowHibernation=no
    AllowSuspendThenHibernate=no
    AllowHybridSleep=no

## How to install Linux kernel 5.19 on Ubuntu 22.04 or 20.04

    ## https://www.how2shout.com/linux/how-to-install-linux-kernal-5-19-on-ubuntu-22-04-or-20-04/
    $ wget https://raw.githubusercontent.com/pimlie/ubuntu-mainline-kernel.sh/master/ubuntu-mainline-kernel.sh
    $ chmod +x ubuntu-mainline-kernel.sh
    $ sudo mv ubuntu-mainline-kernel.sh /usr/local/bin/
    $ ubuntu-mainline-kernel.sh -c
    $ ubuntu-mainline-kernel.sh -r
    $ sudo ubuntu-mainline-kernel.sh -i v5.19.1
    $ sudo ubuntu-mainline-kernel.sh -i
    $ sudo ubuntu-mainline-kernel.sh -l
    $ sudo ubuntu-mainline-kernel.sh -u

## Ubuntu start slow

    $ systemd-analyze blame
    $ snap list

## GDM remove xsessions

    $ cd /usr/share/xsessions
    $ cd /usr/share/wayland-sessions

## Ubuntu 22.04 Desktop

    $ sudo apt install gnome-tweaks gnome-shell-extension gnome-shell-extension-manage
    $ sudo apt install gnome-shell-extension-desktop-icons-ng

## How to Change the Default Terminal Emulator in Ubuntu 22.04

    $ sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/bin/urxvt 1
    $ sudo update-alternatives --config x-terminal-emulator

## How to Change Login Screen Background in Ubuntu 22.04

    ## https://ubuntuhandbook.org/index.php/2022/04/login-screen-background-ubuntu-22-04/
    $ sudo apt install libglib2.0-dev-bin
    $ wget -qO - https://github.com/PRATAP-KUMAR/ubuntu-gdm-set-background/archive/main.tar.gz | tar zx --strip-components=1 ubuntu-gdm-set-background-main/ubuntu-gdm-set-background
    $ sudo ./ubuntu-gdm-set-background --image /PATH/TO/YOUR/IMAGE
    $ sudo ./ubuntu-gdm-set-background --reset
    $ sudo mv ./ubuntu-gdm-set-background /usr/local/bin


## How to MAKE Ubuntu 22.04 Look Like Mac OS Monterey

    ## https://github.com/vinceliuice/WhiteSur-gtk-theme
    ## ./install.sh -c Dark -t blue
    $ sudo ./tweaks.sh -g -b "my picture.jpg" # use the custom background
    $ sudo ./tweaks.sh -g -b default          # use the default background
    $ sudo ./tweaks.sh -g -b blank            # make it blank

## SDDM Display manager

    $ sudo apt install sddm
    $ sudo apt install sddm-theme-maya
    $ ls /usr/share/sddm/themes/
    $ cat /etc/sddm.conf <<EOF
    [Users]
    HideUsers=sys_cert
    [Theme]
    ThemeDir=/usr/share/sddm/themes
    Current=maya
    EOF
    $ sddm-greeter --test-mode --theme /usr/share/sddm/themes/maya

    ## switch to gdm
    $ sudo systemctl start gdm3
    $ sudo dpkg-reconfigure gdm3

## Install SDD chili theme

    ## https://gist.github.com/lboulard/839cefd92fec963fd9a8a22a94ab082e
    $ mkdir -p ~/Downloads
    $ cd Downloads && curl -JL https://github.com/MarianArlt/sddm-chili/archive/0.1.5.tar.gz -o sddm-chili-0.1.5.tar.gz
    $ sudo tar -xzvf ~/Downloads/sddm-chili-0.1.5.tar.gz -C /usr/share/sddm/themes
    $ sudo mv /usr/share/sddm/themes/sddm-chili-0.1.5 /usr/share/sddm/themes/chili
    $ sudo apt install qml-module-qtquick-controls \
        qml-module-qtquick-extras qml-module-qtquick-layouts \
        qml-module-qtgraphicaleffects

    ## Test theme inside session (no error about missing modules shall display)
    $ sddm-greeter --theme /usr/share/sddm/themes/chili
    $ sddm-greeter --test-mode --theme /usr/share/sddm/themes/chili

    $ cat /etc/sddm.conf <<EOF
      [Users]
      HideUsers=user1,user2
      [Theme]
      Current=chili
      EOF

## Hide APT news

    $ sudo pro config set apt_news=false

## How to Fix ‘apt-key’ Deprecation Warning on Ubuntu

    ## Method 1: the sensible way
    $ sudo apt-key list
    pub rsa4096 2020-01-29 [SC]
    8CAE 012E BFAC 38B1 7A93  7CD8 C5E2 2450 0C12 89C0

    $ sudo apt-key export 0C1289C0 | sudo gpg --dearmour -o /etc/apt/trusted.gpg.d/teamviewer.gpg

    ## Method 2: the quick way
    $ cd /etc/apt
    $ sudo cp trusted.gpg trusted.gpg.d

## DebootstrapChroot

    ## https://wiki.ubuntu.com/DebootstrapChroot
    ## https://help.ubuntu.com/community/DebootstrapChroot
    ## Getting and installing debootstrap
    $ wget http://archive.ubuntu.com/ubuntu/pool/main/d/debootstrap/debootstrap_1.0.9~hardy1_all.deb
    $ sudo dpkg --install debootstrap_1.0.9~hardy1_all.deb

    ## Installing and configuring schroot
    $ sudo apt-get install schroot
    $ sudo mkdir /var/chroot # Remember, we assume our chroot is here
    $ sudo editor /etc/schroot/schroot.conf

## Setting up a Ubuntu chroot Environment using debootstrap tool

    ## https://blog.knoldus.com/setting-up-a-ubuntu-chroot-environment-using-debootstrap-tool/
    $ sudo apt update
    $ sudo apt install debootstrap
    $ mkdir newchroot-ubuntu
    $ sudo mount -t proc /proc newchroot-ubuntu/proc
    $ sudo mount --rbind /sys newchroot-ubuntu/sys
    $ sudo mount --rbind /dev newchroot-ubuntu/dev
    $ sudo chroot newchroot-ubuntu /bin/bash
    $ sudo umount newchroot-ubuntu/proc newchroot-ubuntu/sys newchroot-ubuntu/dev
    $ sudo rm -rf newchroot-ubuntu

## Test and deploy individual CVE fixes with UA Client and Landscape

    ## https://ubuntu.com/tutorials/test-and-deploy-individual-cve-fixes-with-ua-client-and-landscape#3-patch-individual-cves-via-the-command-line
    $ sudo ua fix CVE-YYYY-XXXX

    Add an example “ua fix CVE-2021-4034” script
    Title: ua fix CVE-2021-4034

    Code:

    #!/bin/bash
    ua fix CVE-2021-4034
    Run as user: root

    Time limit (seconds): 300

    Access group: Global access

## Using CVEScan

    $ sudo snap install cvescan

    ## https://github.com/canonical/sec-cvescan
    $ sudo apt install python3-apt python3-pip
    $ git clone https://github.com/canonical/sec-cvescan
    $ pip3 install --user sec-cvescan/
    $ ~/.local/bin/cvescan
