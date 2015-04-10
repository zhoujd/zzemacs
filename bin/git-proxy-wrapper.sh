#!/bin/sh
#
# Use socat/connect to proxy git through a SOCKS proxy
# to access git repositories outside Network
#

# http://www.dest-unreach.org/socat/
# http://www.taiyo.co.jp/~gotoh/ssh/connect.c

# Configuration.
_proxy=proxy-prc.intel.com
#_proxy=proxy-shz.intel.com
#_proxy=proxy-ir.intel.com

_proxyport=1080

_connect_flag="y"

echo $1 | grep -E "\.intel\.com$|\.kernel\.org$" > /dev/null 2>&1
if [ $? -eq 0 ] ; then
    exec connect $@
else
    if [ $_connect_flag = "y" ] ; then
        exec connect -S $_proxy:$_proxyport $@
    else
        exec socat STDIO SOCKS4:$_proxy:$1:$2,socksport=$_proxyport
    fi
fi
