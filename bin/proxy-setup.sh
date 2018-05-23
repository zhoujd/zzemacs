#!/bin/sh

### /etc/yum.conf
## proxy=http://username:password@yourproxy:8080

### /etc/apt/apt.conf
## Acquire::http::proxy "http://127.0.0.1:8000";  
## Acquire::ftp::proxy "ftp://127.0.0.1:8000";  
## Acquire::https::proxy "https://127.0.0.1:8000";

echo "Setup proxy .bashrc start ..."

if [ $# != 1 ] ; then
    echo "Usage: `basename $0` [host:port]"
    exit 1
fi

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    SETUP_ROOT=$(cd $(dirname $0) && pwd -W)
else
    SETUP_ROOT=$(cd $(dirname $0) && pwd)
fi

ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)

cat > $ZZEMACS_ROOT/etc/profile.d/99_proxy.sh <<EOF
## This is for bash proxy
export http_proxy=$1
export https_proxy=\$http_proxy
export ftp_proxy=\$http_proxy
export no_proxy=localhost,127.0.0.0/8,::1
EOF

##Git http/https proxy
git config --global http.proxy $1
git config --global https.proxy $1

echo "Setup proxy .bashrc end ..."
