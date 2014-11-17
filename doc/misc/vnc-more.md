VNC More
================

1. Install VNC server and client

        sudo yum install -y tigervnc-server
        sudo yum install -y tigervnc         ==> Applications->Internet->TigerVNC Viewer

2. configure

        vim /etc/sysconfig/vncservers
        VNCSERVERS="1:root "
        VNCSERVERARGS[1]="-geometry 800x600 -nolisten tcp -localhost"

        ## VNC start port is 5900 so 5901 for root

3. Set VNC connect password

        su media
        vncpasswd

4. Start VNC server

        /etc/init.d/vncserver start (stop reload restart etc)
        service vncserver start (stop restart etc)
        chkconfig  vncserver on
        chkconfig --list vncserver

5. Connect VNC remote

        vncviewer
        IP:0  =>for root
        IP:1  =>for user

        IP:5901 => for root
        IP:5902 => for user

        vim /root/.vnc/xstartup
        twm& => "startkde &" or"gnome-session &"
