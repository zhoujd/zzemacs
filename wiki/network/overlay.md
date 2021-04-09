overlay
=======

1. Create an Overlay Network in Ubuntu using Open vSwitch (OVS)
   https://etesami.github.io/2019/10/14/overlay-networks-in-ubuntu.html

        ## VM1
        ubuntu@VM1:~$ sudo apt install openvswitch-switch openvswitch-common
        ubuntu@VM1:~$ sudo ovs-vsctl add-br br-mng
        ubuntu@VM1:~$ sudo ovs-vsctl add-port br-mng intif -- set interface intif type=internal
        ubuntu@VM1:~$ sudo ovs-vsctl add-port br-mng vxlan1 -- set interface vxlan1 type=vxlan options:remote_ip=10.0.0.12 options:key=1025
        ubuntu@VM1:~$ sudo ifconfig intif 192.168.10.11/24 mtu 1450 up

        ## VM2
        ubuntu@VM2:~$ sudo ovs-vsctl add-br br-mng
        ubuntu@VM2:~$ sudo ovs-vsctl add-port br-mng intif -- set interface intif type=internal
        ubuntu@VM2:~$ sudo ovs-vsctl add-port br-mng vxlan1 -- set interface vxlan1 type=vxlan options:remote_ip=10.0.0.11 options:key=1025
        ubuntu@VM2:~$ sudo ifconfig intif 192.168.10.12/24 mtu 1450 up

        ## Test the Connectivity
        ubuntu@VM1:~$ ifconfig
        eth0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 10.0.0.11 netmask 255.255.255.0  broadcast 10.0.0.255
        .
        .
        intif: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1450
        inet 192.168.10.11 netmask 255.255.255.0  broadcast 192.168.10.255

        ubuntu@VM1:~$ iperf3 -s
        ubuntu@VM1:~$ iperf3 -c 192.168.10.11
        ubuntu@VM2:~$ iperf3 -c 192.168.10.11

