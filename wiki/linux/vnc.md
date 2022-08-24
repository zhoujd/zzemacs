VNC More
================

## Install VNC server and client on CentOS

    rpm -qa | grep tigervnc
    sudo yum install -y tigervnc-server
    sudo yum install -y tigervnc         ==> Applications->Internet->TigerVNC Viewer

    rpm -qc tigervnc-server  =>/etc/sysconfig/vncservers
    chkconfig --add vncserver
    chkconfig --level 35 vncserver on

    su - media
    vncpasswd
    vncserver
    vncserver -list
    vncserver -kill :1

    ## close firewall
    sudo systemctl stop firewalld.service   # on CentOS7
    sudo systemctl disable firewalld.service

    ===============================
    systemctl stop firewalld.service
    yum install tigervnc-server tigervnc-server-module
    cp /lib/systemd/system/vncserver@.service /etc/systemd/system/vncserver@:1.service
    cd /etc/system/system
    vim vncserver@:1.service
    [Unit]
    Description=Remote desktop service (VNC)
    After=syslog.target network.target
    [Service]
    Type=forking
    User=root
    ExecStart=/usr/bin/vncserver :1 -geometry 1280x1024 -depth 16 -securitytypes=none -fp /usr/share/X11/fonts/misc
    ExecStop=/usr/bin/vncserver -kill :1
    [Install]
    WantedBy=multi-user.target

    systemctl enable vncserver@:1.service
    vncpasswd
    systemctl start vncserver@:1.service
    systemctl status vncserver@:1.service
    netstat -lnt | grep 590*
    grep vnc /var/log/messages
    =================================

    vim /etc/sysconfig/iptables
    -A INPUT -m state --state NEW -m tcp -p tcp --dport 5901 -j ACCEPT
    -A INPUT -m state --state NEW -m tcp -p tcp --dport 5902 -j ACCEPT

    vncviewer -via media@172.31.0.128 localhost:2
    vncpasswd virtual
    vncviewer -passwd  /path/to/server-passwd-file -via media@172.31.0.128 localhost:2

## Configure

    sudo vim /etc/sysconfig/vncservers
    VNCSERVERS="1:root 2:media"
    VNCSERVERARGS[1]="-geometry 800x600 -localhost"
    VNCSERVERARGS[2]="-geometry 1024x768 -localhost"

    ## VNC start port is 5900 so 5901 for root

## Set VNC connect password

    su media
    vncpasswd

## Start VNC server

    sudo /etc/init.d/vncserver start (stop reload restart etc)
    sudo service vncserver start (stop restart etc)
    sudo chkconfig vncserver on
    sudo chkconfig --list vncserver

## Connect VNC remote

    vncviewer
    IP:1  =>for root
    IP:2  =>for media

    IP:5901 => for root
    IP:5902 => for media

    sudo vim /root/.vnc/xstartup
    twm& => "startkde &" or"gnome-session &"

## VNC on Windows

    ## http://www.tightvnc.com/download.php

## Download x11vnc from http://www.karlrunge.com/x11vnc/

    $ export TZ=America/Los_Angeles
    $ ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

    $ sudo apt install x11vnc xvfb

    $ mkdir -p ~/.vnc
    $ x11vnc -storepasswd 1234 ~/.vnc/passwd
    $ x11vnc -forever -usepw -create

    $ sudo x11vnc -storepasswd /etc/x11vnc.pass
    $ sudo chmod 755 /etc/x11vnc.pass
    $ x11vnc -auth guess -rfbauth /etc/x11vnc.pass -rfbport 5900 -forever -display :0

    ## Firewall on Ubuntu
    $ sudo ufw allow 5900
    $ sudo ufw allow OpenSSH
    $ sudo ufw allow 5901:5910/tcp
    or
    $ sudo ufw disable

## VNC on Ubuntu 14.04

    $ sudo apt-get install vnc4server
    $ sudo apt-get install gnome-panel gnome-settings-daemon metacity nautilus gnome-terminal
    $ cat .vnc/xstartup
    #!/bin/sh
    export XKL_XMODMAP_DISABLE=1
    unset SESSION_MANAGER
    unset DBUS_SESSION_BUS_ADDRESS

    gnome-panel &
    gnome-settings-daemon &
    metacity &
    nautilus &
    gnome-terminal &

## VNC set geometry for client

    ## on sever side
    $ vncserver -geometry 1280x720 :1

## Client on Ubuntu

    ## default VNC client for Ubuntu: Remmina
    $ sudo apt-get install remmina

## VNC Viewer on Windows

    ## https://www.realvnc.com/en/connect/download/vnc/
    ## remove "Sign in" button
    File -> Preference -> Expert -> AllowSignin => false

## VNCServer on Ubuntu

    ## https://www.digitalocean.com/community/tutorials/how-to-install-and-configure-vnc-on-ubuntu-20-04
    ## Step 1 — Installing the Desktop Environment and VNC Server
    $ sudo apt update
    $ sudo apt install xfce4 xfce4-goodies
    $ sudo apt install tightvncserver
    $ vncserver
    $ vncpasswd

    ## Step 2 — Configuring the VNC Server
    $ vncserver -kill :1
    $ mv ~/.vnc/xstartup ~/.vnc/xstartup.bak
    $ nano ~/.vnc/xstartup
    #!/bin/bash
    xrdb $HOME/.Xresources
    startxfce4 &
    $ chmod +x ~/.vnc/xstartup

    ## https://unix.stackexchange.com/questions/398905/vnc-server-only-listening-for-connections-from-localhost
    ## Binds the VNC server to loopback interface
    ## Only allow connections that originate from the server on which it’s installed
    ## [-localhost yes|no]    Only accept VNC connections from localhost
    $ vncserver -localhost
    $ vncserver -localhost no

    ## Step 3 — Connecting to the VNC Desktop Securely
    $ ssh -L 59000:localhost:5901 -C -N -l user your_server_ip
    ## PuTTY Reconfiguration window. Expand the SSH branch and click on Tunnels.
    ## On the Options controlling SSH port forwarding screen,
    ## enter 59000 as the Source Port and localhost:5901 as the Destination,

    ## Step 4 — Running VNC as a System Service
    $ sudo nano /etc/systemd/system/vncserver@.service
    [Unit]
    Description=Start TightVNC server at startup
    After=syslog.target network.target

    [Service]
    Type=forking
    User=user
    Group=user
    WorkingDirectory=/home/user

    PIDFile=/home/user/.vnc/%H:%i.pid
    ExecStartPre=-/usr/bin/vncserver -kill :%i > /dev/null 2>&1
    ExecStart=/usr/bin/vncserver -depth 24 -geometry 1920x1080 -localhost no :%i
    ExecStop=/usr/bin/vncserver -kill :%i

    [Install]
    WantedBy=multi-user.target

    $ sudo systemctl daemon-reload
    $ sudo systemctl enable vncserver@1.service
    $ vncserver -kill :1
    $ sudo systemctl start vncserver@1
    $ sudo systemctl status vncserver@1

## Use vncviewer on Ubuntu

    $ sudo apt install tigervnc-viewer
    $ vncviewer localhost:1
