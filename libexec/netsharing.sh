#!/bin/bash
## Sharing an Internet connection
## eth0 is the interface which is connected to the Internet
## wlan0 is the wireless interface which is supposed to share the Internet to other devices
## netsharing.sh eth0 wlan0

echo 1 > /proc/sys/net/ipv4/ip_forward

iptables -A FORWARD -i $1 -o $2 -s 10.99.0.0/16 -m conntrack --ctstate NEW -j ACCEPT
iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
iptables -A POSTROUTING -t nat -j MASQUERADE

echo "$0 $1 $2 done"
