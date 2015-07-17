#!/bin/sh

### /etc/yum.conf
## proxy=http://username:password@yourproxy:8080

### /etc/apt/apt.conf
## Acquire::http::proxy "http://127.0.0.1:8000";  
## Acquire::ftp::proxy "ftp://127.0.0.1:8000";  
## Acquire::https::proxy "https://127.0.0.1:8000";

echo "Setup self .bashrc start ..."
echo "Usage: `basename $0` [host:port]"

##Check run OS
if [ "$OS" = "Windows_NT" ] ; then
    SETUP_ROOT=$(cd $(dirname $0) && pwd -W)
else
    SETUP_ROOT=$(cd $(dirname $0) && pwd)
fi

##Import vars and functions
. $SETUP_ROOT/sample.sh

ZZEMACS_ROOT=$(cd $SETUP_ROOT/.. && pwd)

cat > $ZZEMACS_ROOT/etc/profile.d/99_proxy.sh <<EOF
## This is for bash proxy
export http_proxy=$1
export https_proxy=\$http_proxy
export ftp_proxy=\$http_proxy
EOF

echo "Setup proxy .bashrc end ..."
