Proxy setting
==============

## Bash proxy setting

    cat .bashrc
    cat /etc/profile

    export http_proxy=user:pwd@proxy_addr:port
    export https_proxy=
    export ftp_proxy=
    export no_proxy=10.0.0.0/8,192.168.0.0/16,localhost,127.0.0.0/8,134.134.0.0/16

## Ubuntu apt-get and apt

    cat /etc/apt/atp.conf

    Acquire::http::proxy  "http://$proxyserveraddr:$proxyserverport/";
    Acquire::https::proxy "https://$proxyserveraddr:$proxyserverport/";
    Acquire::ftp::proxy   "ftp://$proxyserveraddr:$proxyserverport/";
    Acquire::socks::proxy "socks://$proxyserveraddr:$proxyserverport/";

## Centos yum

    cat /etc/yum.conf
    proxy=http://$proxyserveraddr:$proxyserverport

## Install And Setup TinyProxy

    ## https://github.com/isabelcosta/testing-tiny-proxy
    $ sudo apt install tinyproxy-bin
    $ /etc/init.d/tinyproxy stop
    $ /etc/init.d/tinyproxy start
    $ /etc/init.d/tinyproxy status

    ## Modify configure file for upstream
    $ sudo vim /etc/tinyproxy/tinyproxy.conf
    upstream http proxy-prc.*****.com:913
    $ /etc/init.d/tinyproxy stop
    $ /etc/init.d/tinyproxy start

    ## systemctl
    $ sudo systemctl restart tinyproxy.service
    $ sudo systemctl status tinyproxy.service

    ## firewall
    $ sudo ufw allow 8888

    ## Test tinyproxy
    $ curl -v --proxy http://127.0.0.1:8888 www.baidu.com
    $ IP=127.0.0.1
    $ curl -v --proxy http://${IP}:8888 www.baidu.com
