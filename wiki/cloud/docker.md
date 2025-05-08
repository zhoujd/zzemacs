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

    $ host=localhost
    $ port=913
    $ mkdir -p ~/.docker
    $ tee ~/.docker/config.json <<EOF
    {
      "proxies":
      {
       "default":
       {
         "httpProxy": "http://$host:$port",
         "httpsProxy": "http://$host:$port",
         "noProxy": "127.0.0.0/8,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"
       }
      }
    }
    EOF

## Docker proxy for host pull images

    $ host=localhost
    $ port=913
    $ sudo mkdir /etc/systemd/system/docker.service.d
    $ sudo tee /etc/systemd/system/docker.service.d/http-proxy.conf <<EOF
    [Service]
    Environment="HTTP_PROXY=http://$host:$port"
    Environment="HTTPS_PROXY=http://$host:$port"
    Environment="FTP_PROXY=http://$host:$port"
    Environment="NO_PROXY=.intel.com,intel.com,localhost,127.0.0.0/8,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"
    EOF

    $ sudo systemctl daemon-reload
    $ sudo systemctl restart docker
    $ sudo systemctl show --property=Environment docker
    $ sudo docker run hello-world

## Docker daemon example

    $ sudo tee /etc/docker/daemon.json <<EOF
    {
        "insecure-registries": ["$TOOLS_SERVER:5000"],
        "tls": false,
        "storage-driver": "overlay2"
    }
    EOF

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

## How to Manually Create A Container Network - Using Linux Network Namespaces and Veth Pair

    # Create network namespace.
    ubuntu@vm0:~$ ip netns add sample

    # List namespaces.
    ubuntu@vm0:~$ ip netns list
    sample

    # Create new veth pair.
    ubuntu@vm0:~$ ip link add veth1 type veth peer name veth2

    # List network interfaces.
    ubuntu@vm0:~$ ip a

    # Move veth1 to 'sample' namespace.
    ubuntu@vm0:~$ ip link set veth1 netns sample

    # List interfaces in 'default' namespace.
    ubuntu@vm0:~$ ip a

    # List interfaces in 'default' namespace.
    ubuntu@vm0:~$ ip a

    # Create a linux bridge *samplebr*.
    ubuntu@vm0:~$ ip link add name samplebr type bridge

    # Join *veth2* network interface to *samplebr* bridge.
    ubuntu@vm0:~$ ip link set veth2 master samplebr

    # Assign an ipv4 address to *samplebr*.
    ubuntu@vm0:~$ ip addr add 10.1.1.1/24 brd + dev samplebr

    # Assign an ipv4 address to *veth1*.
    ubuntu@vm0:~$ ip netns exec sample ip addr add 10.1.1.2/24 dev veth1

    # Up *veth1* device.
    ubuntu@vm0:~$ ip netns exec sample ip link set veth1 up

    # Up container localhost.
    ubuntu@vm0:~$ ip netns exec sample ip link set lo up

    # Up *veth2* device.
    ubuntu@vm0:~$ ip link set veth2 up

    # Up *samplebr* device.
    ubuntu@vm0:~$ ip link set samplebr up

    # Add bridge 'samplebr' as default gateway for the container network.
    ubuntu@vm0:~$ ip netns exec sample ip route add default via 10.1.1.1
    ubuntu@vm0:~$ ping 10.1.1.2
    PING 10.1.1.1 (10.1.1.2) 56(84) bytes of data.
    64 bytes from 10.1.1.2: icmp_seq=1 ttl=64 time=0.022 ms
    64 bytes from 10.1.1.2: icmp_seq=2 ttl=64 time=0.028 ms

    ubuntu@vm0:~$ ip netns exec sample ping 172.17.52.174
    PING 172.17.52.174 (172.17.52.174) 56(84) bytes of data.
    64 bytes from 172.17.52.174: icmp_seq=1 ttl=64 time=0.024 ms
    64 bytes from 172.17.52.174: icmp_seq=2 ttl=64 time=0.143 ms

    ubuntu@vm0:~$ sudo ip netns exec sample ping 8.8.8.8
    connect: Network is unreachable

    ubuntu@vm0:~$ ip netns exec sample ping github.com
    ping: github.com: Temporary failure in name resolution

    # Make sure *ip_forwarding* is enabled.
    root@test1:~# sysctl -w net.ipv4.ip_forward=1

    # Enable sending requests and getting responses to/from internet (ping 8.8.8.8).
    root@test1:~# iptables -t nat -A POSTROUTING -s 10.1.1.0/24 ! -o samplebr -j MASQUERADE

    # Find your dns nameserver for the host network interface (eth0)
    root@test1:~#  systemd-resolve --status

    # Create resolv.conf file for container network.
    root@test1:~# mkdir -p /etc/netns/sample/
    root@test1:~# echo "nameserver 172.17.52.161" > /etc/netns/sample/resolv.conf

    # Test dns again.
    root@test1:~# ip netns exec sample ping github.com
    PING github.com (140.82.118.3) 56(84) bytes of data.
    64 bytes from lb-140-82-118-3-ams.github.com (140.82.118.3): icmp_seq=1 ttl=49 time=28.5 ms
    64 bytes from lb-140-82-118-3-ams.github.com (140.82.118.3): icmp_seq=2 ttl=49 time=27.4 ms

## Commit Changes to Image

    $ sudo docker commit [CONTAINER_ID] [new_image_name]

## Linux kernel capabilities are restricted within containers

    ## https://dockerlabs.collabnix.com/advanced/security/capabilities/
    ## https://man7.org/linux/man-pages/man7/capabilities.7.html
    ## With “CAP_”. For example, CAP_CHOWN, CAP_NET_ADMIN, CAP_SETUID, CAP_SYSADMIN etc.
    ## By default, the capabilities below are applied to containers:
       AUDIT_WRITE
       CHOWN
       DAC_OVERRIDE
       FOWNER
       FSETID
       KILL
       MKNOD
       NET_BIND_SERVICE
       NET_RAW
       SETFCAP
       SETGID
       SETPCAP
       SETUID
       SYS_CHROOT

    ## To drop capabilities from the root account of a container
    $ sudo docker run --rm -it --cap-drop $CAP alpine sh

    ## To add capabilities to the root account of a container
    $ sudo docker run --rm -it --cap-add $CAP alpine sh

    ## To drop all capabilities and then explicitly add individual capabilities to the root account of a container
    $ sudo docker run --rm -it --cap-drop ALL --cap-add $CAP alpine sh

    ## Testing Docker capabilities
    $ docker container run --rm -it alpine chown nobody /
    $ docker container run --rm -it --cap-drop ALL --cap-add CHOWN alpine chown nobody /

    ## With docker-compose.
    This works for version 2 and 3. For example:
    ---
    version: '2'
    services:
      myapp:
        cap_add:
        - SYS_ADMIN
        - DAC_READ_SEARCH

    ## https://kubernetes.io/docs/tasks/configure-pod-container/security-context/
    apiVersion: v1
    kind: Pod
    metadata:
      name: security-context-demo-4
    spec:
      containers:
      - name: sec-ctx-4
        image: gcr.io/google-samples/node-hello:1.0
        securityContext:
          capabilities:
            add: ["NET_ADMIN", "SYS_TIME"]

## Save/load container using tgz file (tar.gz)

    ## for not running docker, use save:
    docker save <dockernameortag> | gzip > mycontainer.tgz

    ## for running or paused docker, use export:
    docker export <dockernameortag> | gzip > mycontainer.tgz

    ## load from save
    gunzip -c mycontainer.tgz | docker load

    ## load from export
    gunzip -c mycontainer.tgz | docker import - <image:tag>

## Save all the images is like this

    ## save all images to tar
    $ docker save $(docker images --format '{{.Repository}}:{{.Tag}}') -o allimages.tar
    $ docker load -i allimages.tar

    ## save all images to tar.gz
    $ OUT=$(docker images --format '{{.Repository}}:{{.Tag}}')
    $ OUTPUT=($OUT)
    $ docker save $(echo "${OUTPUT[*]}") -o /dir/images.tar
    $ docker save $(echo "${OUTPUT[*]}") | gzip > images.tar.gz

    ## load from tar.gz
    $ tar xvf images.tar.gz -O | docker load

## Install Docker Engine on Ubuntu

    ## Set up the repository
    $ sudo apt update
    $ sudo apt install ca-certificates curl gnupg lsb-release
    $ sudo mkdir -p /etc/apt/keyrings
    $ curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
    $ echo \
      "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
      $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

    ## Install Docker Engine
    $ sudo apt update
    $ sudo apt install docker-ce docker-ce-cli containerd.io docker-compose-plugin

    ## To install a specific version
    $ apt-cache madison docker-ce
    $ sudo apt install docker-ce=<VERSION_STRING> docker-ce-cli=<VERSION_STRING> containerd.io docker-compose-plugin

    ## Verify that Docker Engine
    $ sudo service docker start
    $ sudo docker run hello-world

    ## Uninstall Docker Engine
    $ sudo apt-get purge docker-ce docker-ce-cli containerd.io docker-compose-plugin
    $ sudo rm -rf /var/lib/docker
    $ sudo rm -rf /var/lib/containerd

## Proxy docker /var/run/docker.sock to port 2375 with socat

    $ docker run -d \
      --volume /var/run/docker.sock:/var/run/docker.sock \
      --name docker-http \
      deb socat -d -d TCP-L:2375,fork UNIX:/var/run/docker.sock
    DOCKER_URL=$(docker inspect -f "{{.NetworkSettings.IPAddress}}" docker-http):2375
    curl $DOCKER_URL/_ping

## Podman

    ## https://podman.io/getting-started/
    ## https://podman.io/getting-started/installation
    $ sudo apt install podman
    $ podman --help


## Network docker0 in iptables

    $ sudo iptables-save | grep -i docker
    :DOCKER - [0:0]
    :DOCKER-ISOLATION-STAGE-1 - [0:0]
    :DOCKER-ISOLATION-STAGE-2 - [0:0]
    :DOCKER-USER - [0:0]
    -A FORWARD -j DOCKER-USER
    -A FORWARD -j DOCKER-ISOLATION-STAGE-1
    -A FORWARD -o docker0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    -A FORWARD -o docker0 -j DOCKER
    -A FORWARD -i docker0 ! -o docker0 -j ACCEPT
    -A FORWARD -i docker0 -o docker0 -j ACCEPT
    -A DOCKER-ISOLATION-STAGE-1 -i docker0 ! -o docker0 -j DOCKER-ISOLATION-STAGE-2
    -A DOCKER-ISOLATION-STAGE-1 -j RETURN
    -A DOCKER-ISOLATION-STAGE-2 -o docker0 -j DROP
    -A DOCKER-ISOLATION-STAGE-2 -j RETURN
    -A DOCKER-USER -j RETURN
    :DOCKER - [0:0]
    -A PREROUTING -m addrtype --dst-type LOCAL -j DOCKER
    -A OUTPUT ! -d 127.0.0.0/8 -m addrtype --dst-type LOCAL -j DOCKER
    -A POSTROUTING -s 172.17.0.0/16 ! -o docker0 -j MASQUERADE
    -A DOCKER -i docker0 -j RETURN

## Creating a multi-stage dockerfile

    ## Define multible FROM in Dockerfile
    ## It gets last stage image, and this reduces the image size
    $ docker build -<<EOF
    FROM centos：7.0.3 AS build_base
    ...
    COPY project /build/
    WORKDIR /build
    ENTRYPOINT ["/build/server"]

    FROM centos：7.0.3
    COPY --from build_base ***.so .
    COPY --from build_base /build/server .
    ENV LD_LIBRARY_PATH=./
    EOF

## GDB debug in Docker container

    ## (gdb) attach 30721
    ## Attaching to process 30721
    ## ptrace: Operation not permitted.

    ## 1. Close seccomp
    $ docker run --security-opt seccomp=unconfined

    ## 2. Priviledge mode
    $ docker run --priviledged

    ## 3. Only open pstrace limitation (prefer)
    $ docker run --cap-add sys_pstrace

## Inside a docker container on a remote host

    C-x C-f /docker:redis_container:/
    C-x C-f /ssh:root@ssb.willschenk.com|docker:ssb-pub:/

## Delete all dangling volumes

    $ docker volume rm $(docker volume ls -qf dangling=true)
    $ docker volume prune

## Tell the terminal in the container its size

    $ docker exec -it -e COLUMNS="`tput cols`" -e LINES="`tput lines`" container bash

## Running a window manager from inside a docker container

    ## https://wiki.archlinux.org/title/Xephyr
    ## http://blog.csicar.de/docker/window-manger/2016/05/24/docker-wm.html
    $ sudo pacman -S xorg-server-xephyr
    or
    $ sudo apt-get install xserver-xephyr
    $ Xephyr :1 -ac -br -screen 1024x768 -resizeable -reset -terminate &
    $ docker run -it -e DISPLAY=:1 --device /dev/snd -v /tmp/.X11-unix:/tmp/.X11-unix csicar/ubuntu-mate-desktop /usr/bin/mate-session

    ## https://askubuntu.com/questions/175902/remote-x-server-with-ssh-x
    $ Xephyr :1 -screen 1024x768 -query 192.168.1.107
    $ ssh -X username@192.168.1.107 Xephyr :1 -query localhost -screen 1280x1024

## Running Xephyr inside a docker container-docker

    $ docker run -e DISPLAY=$DISPLAY -v /tmp:/tmp --ipc=host --pid=host zz/ubuntu-20.04-zwm:dev xephyr :103
    $ docker run -e DISPLAY=$DISPLAY -v /tmp:/tmp zz/ubuntu-20.04-zwm:dev Xephyr :103

## Install docker buildx

    ## https://docs.docker.com/build/buildkit/
    ## https://github.com/docker/buildx/releases/
    ## https://github.com/docker/buildx
    $ VERSION=v0.14.1
    $ mkdir -p $HOME/.docker/cli-plugins
    $ wget https://github.com/docker/buildx/releases/download/$VERSION/buildx-$VERSION.linux-amd64 -O $HOME/.docker/cli-plugins/docker-buildx
    $ chmod +x $HOME/.docker/cli-plugins/docker-buildx
    $ export DOCKER_BUILDKIT=1
    $ export COMPOSE_DOCKER_CLI_BUILD=1
    $ echo 'export DOCKER_BUILDKIT=1' >> $HOME/.profile
    $ echo 'export COMPOSE_DOCKER_CLI_BUILD=1' >> $HOME/.profile

## How to enable multi-platform Docker builds on Ubuntu 22.04

    ## https://www.staldal.nu/tech/2023/02/10/how-to-enable-multi-platform-docker-builds-on-ubuntu-22-04/
    ## Install QEMU’s static user mode emulation binaries:
    $ sudo apt install qemu-user-static

    ## Install Docker Engine with Buildx support according instructions from https://docs.docker.com/engine/install/ubuntu/
    ## do not install docker-ce or docker.io from Ubuntu’s standard repository

    ## Create a Docker Buildx builder:
    $ docker buildx create --name multiplatform --bootstrap --us

    ## Verify it:
    $ docker buildx inspec

    ## Then you can do multi-platform Docker builds like this:
    $ docker buildx build --platform linux/amd64,linux/arm64

## Docker BuildKit

    $ sudo apt install docker-buildx

## Docker locally

    ## Update docker daemon.json
    $ SERVER_IP=localhost
    $ sudo tee /etc/docker/daemon.json <<EOF
    {
        "registry-mirrors":[
            "https://docker.m.daocloud.io"
        ],
        "insecure-registries": [
            "$SERVER_IP:5000"
        ],
        "data-root": "/data/docker/default"
    }
    EOF

## Docker registry

    ## Run a local registry
    $ docker run -d -p 5000:5000 --restart always --name registry registry:2

    ## Edit daemon.json
    $ sudo vim /etc/docker/daemon.json
    {
        "insecure-registries": [
            "$SERVER_IP:5000"
        ],
    }

    ## Restart docker service
    $ sudo systemctl daemon-reload
    $ sudo systemctl restart docker
    $ docker info

    ## Test
    $ docker pull ubuntu
    $ docker tag ubuntu localhost:5000/ubuntu
    $ docker push localhost:5000/ubuntu
