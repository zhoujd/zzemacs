Dnsmasq
=======

1. Setup local DNS Resolver using Dnsmasq on Ubuntu 20.04

        $ sudo apt update -y
        $ sudo systemctl disable --now systemd-resolved
        $ sudo systemctl stop systemd-resolved
        $ rm -rf /etc/resolv.conf
        $ echo "nameserver 8.8.8.8" > /etc/resolv.conf
        
        ##Install Dnsmasq
        $ sudo apt install dnsmasq dnsutils ldnsutils -y
        $ sudo systemctl status dnsmasq
        
        ##Configure Dnsmasq
        $ sudo nano /etc/dnsmasq.conf
        port=53
        domain-needed
        bogus-priv
        listen-address=127.0.0.1,your-server-ip
        expand-hosts
        domain=dns-example.com
        cache-size=1000
        $ sudo nano /etc/resolv.conf
        nameserver your-server-ip
        $ dnsmasq --test
        $ sudo systemctl restart dnsmasq
        $ ss -alnp | grep -i :53
        
        ##Add DNS Records to Dnsmasq Server
        $ nano /etc/hosts
        your-server-ip host1.dns-example.com
        
        ##Verify Dnsmasq Server Resolution
        $ dig host1.dns-example.com +short
        your-server-ip
        $ dig howtoforge.com +short
        172.67.68.93
        104.26.3.165
        104.26.2.165
        
        ##Configure Remote Client to Use Dnsmasq DNS Server
        $ sudo apt install dnsutils ldnsutils -y
        $ nano /etc/resolv.conf
        nameserver your-server-ip
        $ dig host1.dns-example.com
        $ drill google.com | grep "Query time"
        ;; Query time: 290 msec
        $ drill google.com | grep "Query time"
        ;; Query time: 4 msec

2. Configure Dnsmasq as DHCP Server 

        $ sudo vim /etc/dnsmasq.conf
        dhcp-range=192.168.3.25,192.168.3.50,24h
        dhcp-option=option:router,192.168.3.1
        dhcp-option=option:ntp-server,192.168.3.5
        dhcp-option=option:dns-server,192.168.3.5
        dhcp-option=option:netmask,255.255.255.0
        
        $ sudo systemctl restart dnsmasq

3. Dnsmasq in Docker

        ##https://github.com/jpillora/docker-dnsmasq
        $ docker search dnsmasq
        $ docker pull jpillora/dnsmasq
        $ vi /opt/dnsmasq.conf
        $ docker run \
        --name dnsmasq \
        -d \
        -p 53:53/udp \
        -p 8053:8080 \
        -v /opt/dnsmasq.conf:/etc/dnsmasq.conf \
        --log-opt "max-size=100m" \
        -e "HTTP_USER=admin" \
        -e "HTTP_PASS=passwd" \
        --restart always \
        jpillora/dnsmasq
        
        $ firefox http://<docker-host>:5380

        ##test
        $ host myhost.company <docker-host>
        Using domain server:
        Name: <docker-host>
        Address: <docker-host>#53
        Aliases:

        myhost.company has address 10.0.0.2
