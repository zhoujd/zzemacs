kubernetes cni
==============

## HOST-DEVICE CNI

    ## https://www.cni.dev/plugins/current/main/host-device/
    ## https://blog.51cto.com/u_15301988/3080674
    $ ip a
    $ ip netns add test
    $ ip link set dev ens8 netns test
    $ ip netns exec test ip a

    $ cat > myhost-device.conf <<"EOF"
    {
        "cniVersion": "0.3.1",
        "type": "host-device",
        "device": "ens8",
        "name": "host"
    }
    EOF

    $ export CNI_COMMAND=ADD
    $ export CNI_NETNS=/var/run/netns/test
    $ export CNI_IFNAME=eth0
    $ export CNI_ARGS
    $ export CNI_PATH=/opt/cni/bin
    $ export CNI_CONTAINERID="aaa"
    $ /opt/cni/bin/host-device < myhost-device.conf
    $ ip netns exec test ip a

## How to View the Network Namespaces in Kubernetes

    ## Get Container ID
    $ docker ps
    ## Get Container PID
    $ docker inspect --format '{{ .State.Pid }}' <CONTAINER_ID>
    ## Run Command in Namespace
    $ nsenter -t <CONTAINER_PID> -n ip addr

    ## Move NIC to container network space
    $ ip link set dev <NIC> netns <CONTAINER_PID>
    $ nsenter -t <CONTAINER_PID> -n dhclient <NIC>
    $ nsenter -t <CONTAINER_PID> -n ip a

    ## Move back to system PID=1
    $ nsenter -t <CONTAINER_PID> -n
    $ lsns -p <CONTAINER_PID>
    $ lsns -p 1
    $ ip link set dev <NIC> netns 1

    ## Back to default ns
    $ exit
