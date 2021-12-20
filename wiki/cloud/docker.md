Docker
======

## Container entrypoint

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

## Add container network route

    $ docker ps | grep <app-name> | awk '{print $1}'
    2a808c0b24a7
    $ docker inspect -f {{.State.Pid}} 2a808c0b24a7
    63011
    $ nsenter -n -t 63011
    Revert complete!
    $ route add -net 195.168.1.0 netmask 255.255.255.0 gw 6.6.6.1
    $ route
    $ exit

## Docker proxy for fetching package when building docker image

    $ cat > ~/.docker/config.json <<EOF
    {
     "proxies":
     {
       "default":
       {
         "httpProxy": "http://<host>:<port>",
         "httpsProxy": "http://<host>:<port>",
         "noProxy": "127.0.0.0/8"
       }
      }
    }
    EOF

## Docker proxy for host pull images

    local host=child-prc.intel.com
    local port=913

    $ sudo mkdir /etc/systemd/system/docker.service.d
    $ sudo tee /etc/systemd/system/docker.service.d/http-proxy.conf <<EOF
    [Service]
    Environment="HTTP_PROXY=http://$host:$port"
    Environment="HTTPS_PROXY=http://$host:$port"
    Environment="FTP_PROXY=http://$host:$port"
    Environment="NO_PROXY=.intel.com,intel.com,localhost,127.0.0.0/8,10.0.0.0/8"
    EOF

## Docker daemon example

    $ sudo tee /etc/docker/daemon.json <<EOF
    {
        "insecure-registries": ["$TOOLS_SERVER:5000"],
        "tls": false,
        "storage-driver": "overlay2"
    }
    EOF

## Docker restart service

    $ sudo systemctl daemon-reload
    $ sudo systemctl restart docker
    $ sudo systemctl show --property=Environment docker
    $ sudo docker run hello-world

## How Docker Container Networking Works - Mimic It Using Linux Network Namespaces

    ## https://dev.to/polarbit/how-docker-container-networking-works-mimic-it-using-linux-network-namespaces-9mj
    ## Try to list docker network namespaces. But the result will be empty.
    $ sudo ip netns list
    <no result>

    ## Make docker network namespaces visible.
    $ sudo mkdir -p /var/run/netns
    $ pid1="$(docker inspect con1 -f '{{.State.Pid}}')"
    $ pid2="$(docker inspect con2 -f '{{.State.Pid}}')"
    $ pid3="$(docker inspect con3 -f '{{.State.Pid}}')"
    $ sudo ln -sf /proc/$pid1/ns/net /var/run/netns/con1
    $ sudo ln -sf /proc/$pid2/ns/net /var/run/netns/con2
    $ sudo ln -sf /proc/$pid3/ns/net /var/run/netns/con3

    ## Now we can see the container network namespaces.
    $ sudo ip netns list
    con3 (id: 3)
    con2 (id: 2)
    con1 (id: 1)
