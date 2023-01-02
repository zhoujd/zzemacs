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
