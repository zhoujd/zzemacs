tunnel
======

## Create a GRE tunnel
    Host A: 192.168.233.204
    Host B: 172.168.10.25

### make sure that ip_gre is loaded

    $ sudo modprobe ip_gre
    $ lsmod | grep gre
    ip_gre                 22432  0
    gre                    12989  1 ip_gre

### On Host A

    $ sudo ip tunnel add gre0 mode gre remote 172.168.10.25 local 192.168.233.204 ttl 255
    $ sudo ip link set gre0 up
    $ sudo ip addr add 10.10.10.1/24 dev gre0

    $ ip route show
    default via 135.112.29.1 dev eth0  proto static
    10.10.10.0/24 dev gre0  proto kernel  scope link  src 10.10.10.1

### On Host B

    $ sudo ip tunnel add gre0 mode gre remote 192.168.233.204 local 172.168.10.25 ttl 255
    $ sudo ip link set gre0 up
    $ sudo ip addr add 10.10.10.2/24 dev gre0

### On Host A

    $ ping 10.10.10.2
    PING 10.10.10.2 (10.10.10.2) 56(84) bytes of data.
    64 bytes from 10.10.10.2: icmp_req=1 ttl=64 time=0.619 ms
    64 bytes from 10.10.10.2: icmp_req=2 ttl=64 time=0.496 ms
    64 bytes from 10.10.10.2: icmp_req=3 ttl=64 time=0.587 ms

### Tear down the existing GRE tunnel

    $ sudo ip link set gre0 down
    $ sudo ip tunnel del gre0
