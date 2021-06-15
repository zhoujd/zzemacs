Proxy setting
==============

## bash proxy setting

    cat .bashrc
    cat /etc/profile

    export http_proxy=user:pwd@proxy_addr:port
    export https_proxy=
    export ftp_proxy=
    export no_proxy=10.0.0.0/8,192.168.0.0/16,localhost,127.0.0.0/8,134.134.0.0/16

## ubuntu apt-get

    cat /etc/apt/atp.conf

    Acquire::http::proxy  "http://$proxyserveraddr:$proxyserverport/";
    Acquire::https::proxy "https://$proxyserveraddr:$proxyserverport/";
    Acquire::ftp::proxy   "ftp://$proxyserveraddr:$proxyserverport/";
    Acquire::socks::proxy "socks://$proxyserveraddr:$proxyserverport/";

## centos yum

    cat /etc/yum.conf
    proxy=http://$proxyserveraddr:$proxyserverport
