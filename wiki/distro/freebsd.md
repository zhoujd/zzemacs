FreeBSD
==========

## How to install FreeBSD

    ##http://www.freebsd.org/doc/zh_CN/books/handbook/bsdinstall.html

## user -> root

    $ pw groupmod wheel -m <username>
    $ pw user mod <username> -g wheel

    $ cat /etc/group
    wheel:*:0:root,username

## Configure proxy
   If your FreeBSD box works behind a proxy, you may need to configure proxy to make it access network.
   For csh or tcsh, set proxy in /etc/csh.cshrc:

    # setenv HTTP_PROXY http://web-proxy.xxxxxx.com:8080
    # setenv HTTPS_PROXY https://web-proxy.xxxxxx.com:8080

   For sh, set proxy in /etc/profile:

    # export HTTP_PROXY=http://web-proxy.xxxxxx.com:8080
    # export HTTPS_PROXY=https://web-proxy.xxxxxx.com:8080

## Update FreeBSD System

    # freebsd-update fetch
    # freebsd-update install

    # pkg update
    # pkg upgrade

## Install Editors and Bash

    # pkg install nano bash bash-completion

## Secure SSH on FreeBSD

    # nano /etc/ssh/sshd_config
    PermitRootLogin yes

    # service sshd restart

## Install and Configure Sudo on FreeBSD

    # pkg install sudo
    # cat /usr/local/etc/sudoers.d/jiandon
    ## jiandon
    Defaults env_keep+="http_proxy https_proxy ftp_proxy no_proxy socks_proxy all_proxy"
    jiandon ALL=(ALL) ALL

## Manage FreeBSD Services

    # service -e
    # service -l
    # cat /etc/rc.conf
    sshd_enable=”YES”

    or

    # sysrc sshd_enable=”YES”
    # sysrc syslogd_flags="-ss"
    # service syslogd restart

## List Network Sockets

    # sockstat -4
    # sockstat -6
    # sockstat -4 -6
    # sockstat -c
    # sockstat -l
    # pkg install lsof
    # lsof -i4 -i6
    # netstat -an |egrep 'Proto|LISTEN'
    # netstat -a |egrep 'Proto|LISTEN'

## Configure FreeBSD Static IP

    # cat /etc/rc.conf
    #ifconfig_em0="DHCP"
    ifconfig_em0="inet 192.168.1.100 netmask 255.255.255.0"
    #Default Gateway
    defaultrouter="192.168.1.1"

## Configure FreeBSD DNS Network

    # cat /etc/resolv.conf
    nameserver your_first_DNS_server_IP
    nameserver your_second_DNS_server_IP
    search your_local_domain

    # cat /etc/rc.conf
    hostname=”freebsdhost”

    ## multiple IP address
    # cat /etc/rc.conf
    ifconfig_em0_alias0="192.168.1.5 netmask 255.255.255.255"

    # service netif restart

## Install Editors and Bash

    # pkg install nano bash bash-completion
    # cat /etc/shells
    # chsh -s /bin/csh /usr/local/bin/bash
    # env
