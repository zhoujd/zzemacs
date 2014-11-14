#!/bin/sh
#
# Use socat to proxy git through a SOCKS proxy
# to access git repositories outside Network
#

# http://www.dest-unreach.org/socat/

# Configuration.
#_proxy=proxy-ir.intel.com
#_proxy=proxy-shz.intel.com
_proxy=10.7.211.16
_proxyport=1080
exec socat STDIO SOCKS4:$_proxy:$1:$2,socksport=$_proxyport
