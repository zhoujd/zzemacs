iperf
=====

## Generate traffic with iPerf,

    ## http://www.iperf.fr/
    ## http://iperf.fr/download/iperf_2.0.5/iperf-2.0.5-2-win32.zip
    ## -c makes iPerf run as a client. -s makes it run as a server
    $ iperf.exe -c 172.16.0.1 -u -b 1m -t 10 -p 50000 -l 8k -q 2 -f m -i 2
    $ iperf.exe -s -u -p 50000 -l 8k -q 2 -f m -i 2

    ## server
    $ iperf -s -p 12345 -i 1 -M -f “m”
    ## client
    $ iperf -c 192.168.19.200 -p 12345 -i 1 -t 60 -f “M”

    ## udp test
    $ iperf -s -u -i 1 -p 8000
    $ iperf -u -c 192.168.19.253 -t 60 -i 1 -b 120M -p 8000

## iperf3 between interfaces on same host

    ## https://2kswiki.wordpress.com/2018/09/17/iperf3-between-interfaces-on-same-host/
    ## Add real ip’s to interfaces
    $ ip addr add 192.168.10.1/24 dev ens3f0
    $ ip addr add 192.168.11.1/24 dev ens3f1

    ## Add routes
    $ ip route add 192.168.20.1 dev ens3f1
    $ ip route add 192.168.21.1 dev ens3f0

    ## iptables nat routing, add nat for fake ip -> real ip to trick kernel loopback
    $ iptables -t nat -A POSTROUTING -d 192.168.21.1 -j SNAT --to-source 192.168.20.1
    $ iptables -t nat -A POSTROUTING -d 192.168.20.1 -j SNAT --to-source 192.168.21.1
    $ iptables -t nat -A PREROUTING -d 192.168.20.1 -j DNAT --to-destination 192.168.10.1
    $ iptables -t nat -A PREROUTING -d 192.168.21.1 -j DNAT --to-destination 192.168.11.1

    ## accept connections from ifaces if needed
    $ iptables -A INPUT -i ens3f0 -j ACCEPT
    $ iptables -A INPUT -i ens3f1 -j ACCEPT

    ## arp fuckery
    $ arp -s 192.168.20.1
    $ arp -s 192.168.21.1

    ## bind iperf to the real ip
    $ iperf3 -s -B 192.168.10.1

    ## iperf client on the fake ip
    $ iperf3 -c 192.168.20.1

    ## Note: this comes in handy, but is not needed here.
    $ sysctl -w net.ipv4.ip_nonlocal_bind=1

## iperf on two interfaces in once PC

    ## IPs set to two different subnet
    $ ifconfig eth2 10.50.0.1/24
    $ ifconfig eth3 10.50.1.1/24

    ## NAT transfer between source ip and targer ip
    ## nat source IP 10.50.0.1 -> 10.60.0.1 when going to 10.60.1.1
    $ iptables -t nat -A POSTROUTING -s 10.50.0.1 -d 10.60.1.1 -j SNAT --to-source 10.60.0.1
    ## nat inbound 10.60.0.1 -> 10.50.0.1
    $ iptables -t nat -A PREROUTING -d 10.60.0.1 -j DNAT --to-destination 10.50.0.1
    ## nat source IP 10.50.1.1 -> 10.60.1.1 when going to 10.60.0.1
    $ iptables -t nat -A POSTROUTING -s 10.50.1.1 -d 10.60.0.1 -j SNAT --to-source 10.60.1.1
    ## nat inbound 10.60.1.1 -> 10.50.1.1
    $ iptables -t nat -A PREROUTING -d 10.60.1.1 -j DNAT --to-destination 10.50.1.1

    ## Config route and ARP
    $ ip route add 10.60.1.1 dev eth2
    $ arp -i eth2 -s 10.60.1.1 00:1B:21:C1:F6:0F # eth3's mac address
    $ ip route add 10.60.0.1 dev eth3
    $ arp -i eth3 -s 10.60.0.1 00:1B:21:C1:F6:0E # eth2's mac address

    ## Check network
    $ ping 10.60.1.1

    ## Run iperf
    ## server
    $ iperf -B 10.50.1.1 -s
    ## client: your destination is the other end's fake address
    $ iperf -B 10.50.0.1 -c 10.50.1.1 -t 60 -i 10

    ## Check with tcpdump
    $ tcpdump -nn -i eth2 -c 500
