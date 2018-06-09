#!/bin/sh

### /etc/yum.conf
## proxy=http://username:password@yourproxy:8080

### /etc/apt/apt.conf
## Acquire::http::proxy "http://127.0.0.1:8000";  
## Acquire::ftp::proxy "ftp://127.0.0.1:8000";  
## Acquire::https::proxy "https://127.0.0.1:8000";

if [ $# != 1 ] ; then
    echo "Usage: `basename $0` [host:port]"
    echo "Usage: no prefix-http/https/ftp"
    exit 1
fi

echo "Setup proxy start ..."

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    SETUP_ROOT=$(cd $(dirname $0) && pwd -W)
else
    SETUP_ROOT=$(cd $(dirname $0) && pwd)
fi

ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)

cat > $ZZEMACS_ROOT/etc/profile.d/99_proxy.sh <<EOF
## This is for bash proxy
export http_proxy=http://$1
export https_proxy=http://$1
export ftp_proxy=ftp://$1
export no_proxy=localhost,127.0.0.0/8,::1
EOF

##Git http/https proxy
git config --global http.proxy http://$1
git config --global https.proxy https://$1

echo "Setup proxy end ..."
