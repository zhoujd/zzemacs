VNC More
================

1. Install VNC server and client

        sudo yum install -y tigervnc-server
        sudo yum install -y tigervnc         ==> Applications->Internet->TigerVNC Viewer

        rpm -qc tigervnc-server  =>/etc/sysconfig/vncservers
        chkconfig --level 35 vncserver on

        su - media
        vncpasswd
        vncserver 
        vncserver -list
        vncserver -kill :1

        vim /etc/sysconfig/iptables
        -A INPUT -m state --state NEW -m tcp -p tcp --dport 5901 -j ACCEPT
        -A INPUT -m state --state NEW -m tcp -p tcp --dport 5902 -j ACCEPT

        vncviewer -via media@172.31.0.128 localhost:2
        vncpasswd virtual
        vncviewer -passwd ~/virtual -via media@172.31.0.128 localhost:2

2. configure

        sudo vim /etc/sysconfig/vncservers
        VNCSERVERS="1:root 2:media"
        VNCSERVERARGS[1]="-geometry 800x600 -localhost"
        VNCSERVERARGS[2]="-geometry 1024x768 -localhost"

        ## VNC start port is 5900 so 5901 for root

3. Set VNC connect password

        su media
        vncpasswd

4. Start VNC server

        sudo /etc/init.d/vncserver start (stop reload restart etc)
        sudo service vncserver start (stop restart etc)
        sudo chkconfig vncserver on
        sudo chkconfig --list vncserver

5. Connect VNC remote

        vncviewer
        IP:1  =>for root
        IP:2  =>for media

        IP:5901 => for root
        IP:5902 => for media

        sudo vim /root/.vnc/xstartup
        twm& => "startkde &" or"gnome-session &"
