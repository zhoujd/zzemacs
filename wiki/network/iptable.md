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

## How To List and Delete Iptables Firewall Rules

    ## https://www.digitalocean.com/community/tutorials/how-to-list-and-delete-iptables-firewall-rules
    ## Listing Rules by Specification
    $ sudo iptables -S

    ## Listing a Specific Chain
    $ sudo iptables -S TCP

    ## Listing Rules as Tables
    $ sudo iptables -L
    $ sudo iptables -L INPUT

    ## Showing Packet Counts and Aggregate Size
    $ sudo iptables -L INPUT -v

    ## Resetting Packet Counts and Aggregate Size
    $ sudo iptables -Z
    $ sudo iptables -Z INPUT
    $ sudo iptables -Z INPUT 1

    ## Deleting Rules by Specification
    $ sudo iptables -D INPUT -m conntrack --ctstate INVALID -j DROP

    ## Deleting Rules by Chain and Number
    $ sudo iptables -L --line-numbers
    $ sudo iptables -D INPUT 3

    ## Flushing Chains (delete all rules in a chain)
    ## Flushing a Single Chain
    $ sudo iptables -F INPUT

    ## Flushing All Chains
    $ sudo iptables -F

    ## Flushing All Rules, Deleting All Chains, and Accepting All
    $ sudo iptables -P INPUT ACCEPT
    $ sudo iptables -P FORWARD ACCEPT
    $ sudo iptables -P OUTPUT ACCEPT

    $ sudo iptables -t nat -F
    $ sudo iptables -t mangle -F
    $ sudo iptables -F
    $ sudo iptables -X
