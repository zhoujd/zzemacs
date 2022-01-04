iptables
========

## iptables NAT Port Forwarding

    ## Port 80/TCP
    # iptables -t nat -A PREROUTING -p tcp --dport 80 -j DNAT --to-destination 192.168.0.31:80
    # iptables -t nat -A POSTROUTING -p tcp --dport 80 -j MASQUERADE

    ## Port 443/TCP
    # iptables -t nat -A PREROUTING -p tcp --dport 443 -j DNAT --to-destination 192.168.0.31:443
    # iptables -t nat -A POSTROUTING -p tcp --dport 443 -j MASQUERADE

## Enable NAT With iptables

    ## https://wiki.archlinux.org/title/Internet_sharing#Enable_NAT
    # iptables -t nat -A POSTROUTING -o internet0 -j MASQUERADE
    # iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
    # iptables -A FORWARD -i net0 -o internet0 -j ACCEPT
