x11docker
=========

## URls

    ## https://github.com/mviereck/x11docker
    $ curl -fsSL https://raw.githubusercontent.com/mviereck/x11docker/master/x11docker | sudo bash -s -- --update
    $ docker pull x11docker/xserver
    $ x11docker IMAGENAME [COMMAND]
    $ x11docker x11docker/xfce thunar
    $ x11docker --desktop x11docker/xfce
    $ x11docker --gpu x11docker/xfce glxgears

## nxagent & Xephyr

    ## https://wiki.archlinux.org/title/Xephyr
    $ sudo apt install nxagent
    $ Xephyr -br -ac -noreset -screen 800x600 :1
    $ DISPLAY=:1 xterm
    $ DISPLAY=:1 spectrwm
    $ startx -- /usr/bin/Xephyr :1
