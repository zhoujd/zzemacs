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

    $ host=child-prc.intel.com
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

    $ host=child-prc.intel.com
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

    ## load
    gunzip -c mycontainer.tgz | docker load

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
    $ sudo apt-get update
    $ sudo apt-get install ca-certificates curl gnupg lsb-release
    $ sudo mkdir -p /etc/apt/keyrings
    $ curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
    $ echo \
      "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
      $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

    ## Install Docker Engine
    $ sudo apt-get update
    $ sudo apt-get install docker-ce docker-ce-cli containerd.io docker-compose-plugin

    ## To install a specific version
    $ apt-cache madison docker-ce
    $ sudo apt-get install docker-ce=<VERSION_STRING> docker-ce-cli=<VERSION_STRING> containerd.io docker-compose-plugin

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
