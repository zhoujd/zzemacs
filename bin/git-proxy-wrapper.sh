#!/bin/sh
#
# Use socat/connect to proxy git through a SOCKS proxy
# to access git repositories outside Network

# http://www.dest-unreach.org/socat/
# http://www.taiyo.co.jp/~gotoh/ssh/connect.c

# Check host and port in $SOCKS_PROXY
proxy_host=$(echo $SOCKS_PROXY | awk -F'/' '{print $3}' | awk -F':' '{print $1}')
proxy_port=$(echo $SOCKS_PROXY | awk -F'/' '{print $3}' | awk -F':' '{print $2}')

connect_flag="y"

echo $1 | grep -E "\.intel\.com|\.kernel\.org" > /dev/null 2>&1
if [ $? -eq 0 ] ; then
    exec connect $@
else
    if [ $connect_flag = "y" ] ; then
        exec connect -S $proxy_host:$proxy_port $@
    else
        exec socat STDIO SOCKS4:$proxy_host:$1:$2,socksport=$proxy_port
    fi
fi
