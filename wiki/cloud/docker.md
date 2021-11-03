Docker
======

## Container entrypoint

```bash
tee entrypoint <<EOF
#!/bin/sh -e

cmd=${1:-""}
case ${cmd} in
   *)
      echo "invalid command ${cmd}"
      sleep infinity
      ;;
esac
EOF
```

## Redirecting command output in docker

    $ docker run -it --log-driver=none -a stdin -a stdout -a stderr

    ## When command in background, need use 'wait' to background
    process ending


## Generate the base64-encoded user name and password or token for your mirror registry:

    $ echo -n '<user_name>:<password>' | base64 -w0
    BGVtbYk3ZHAtqXs=


## How can I run a graphical application in a container under Wayland

    ## https://unix.stackexchange.com/questions/330366/how-can-i-run-a-graphical-application-in-a-container-under-wayland
    ## x11
    $ docker run -ti -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix some:ubuntu xclock

    ## wayland
    ## To run Wayland applications in docker without X, you need a running wayland compositor like Gnome-Wayland or Weston.
    ## You have to share the Wayland socket. You find it in XDG_RUNTIME_DIR and its name is stored in WAYLAND_DISPLAY.
    ## As XDG_RUNTIME_DIR only allows access for its owner, you need the same user in container as on host.
    $ docker run -e XDG_RUNTIME_DIR=/tmp \
           -e WAYLAND_DISPLAY=$WAYLAND_DISPLAY \
           -v $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY:/tmp/$WAYLAND_DISPLAY  \
           --user=$(id -u):$(id -g) \
           imagename waylandapplication

    ## xhost
    ## You need one of this ways to allow X applications in docker to access Xwayland (or any X)
    1. Allow your local user access via xhost: xhost +SI:localuser:$(id -un) and create a similar user with docker run option: --user=$(id -u):$(id -g)
    2. Discouraged: Allow root access to X with xhost +SI:localuser:root

## Use x11docker logo Run GUI applications in Docker

    ## https://github.com/mviereck/x11docker
    ## https://github.com/mviereck/x11docker/wiki
    ## https://hub.docker.com/r/x11docker/xwayland
    $ docker pull x11docker/xwayland

## docker-proxy using port when no containers are running

    ## Stop Docker
    $ sudo systemctl stop docker.service

    ## Find your particular zombie proxy processes
    $ sudo netstat -pna | grep docker-proxy
    # tcp6       0      0 :::8025       :::*     LISTEN      <PID_A>/docker-proxy
    # tcp6       0      0 :::13306      :::*     LISTEN      <PID_B>/docker-proxy
    # ...

    ## Kill them
    $ sudo kill -9 PID_A PID_B ...

    ## Restart Docker
    sudo systemctl start docker.service
