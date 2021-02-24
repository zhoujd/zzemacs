FreeBSD
==========

1. How to install FreeBSD9

        ##http://www.freebsd.org/doc/zh_CN/books/handbook/bsdinstall.html

2. user -> root

        $ pw groupmod wheel -m <username>
        $ pw user mod <username> -g wheel

        $ cat /etc/group
        wheel:*:0:root,username

3. Configure proxy
   If your FreeBSD box works behind a proxy, you may need to configure proxy to make it access network.
   For csh or tcsh, set proxy in /etc/csh.cshrc:

        setenv HTTP_PROXY http://web-proxy.xxxxxx.com:8080
        setenv HTTPS_PROXY https://web-proxy.xxxxxx.com:8080
        
   For sh, set proxy in /etc/profile:

        export HTTP_PROXY=http://web-proxy.xxxxxx.com:8080
        export HTTPS_PROXY=https://web-proxy.xxxxxx.com:8080
