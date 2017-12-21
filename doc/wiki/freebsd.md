FreeBSD
==========

1. How to install FreeBSD9

    <http://www.freebsd.org/doc/zh_CN/books/handbook/bsdinstall.html>

2. user -> root

    $ pw groupmod wheel -m <username>
    $ pw user mod <username> -g wheel

    $ cat /etc/group
    wheel:*:0:root,username

