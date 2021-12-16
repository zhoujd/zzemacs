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
