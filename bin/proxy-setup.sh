#!/bin/bash
#set -x

## check arguments
case $# in
    2 )
        HOST=$1
        PORT=$2
        SOCKS_PORT=1080
        ;;
    3 )
        HOST=$1
        PORT=$2
        SOCKS_PORT=$3
        ;;
    * )
        echo "Usage: `basename $0` host port [socks_port]"
        echo "Usage: no prefix-http/ftp"
        exit 1
        ;;
esac

## check os
case "$OS" in
    "Windows_NT" )
        echo "Setup proxy on Windows"
        PROXY_SCRIPT=$HOME/.bashrc.d/99-proxy.sh
        ;;
    * )
        echo "Setup proxy on Linux"
        if [ $EUID -ne 0 ]; then
            echo "You must be a root user" 2>&1
            exit 1
        fi

        PROXY_SCRIPT=/etc/profile.d/zz-proxy.sh
        ;;
esac

tee $PROXY_SCRIPT <<EOF
#!/bin/sh

HOST=$HOST
PORT=$PORT
SOCKS_PORT=$SOCKS_PORT

export http_proxy=http://\$HOST:\$PORT/
export HTTP_PROXY=\$http_proxy

export https_proxy=\$http_proxy
export HTTPS_PROXY=\$https_proxy

export ftp_proxy=\$http_proxy
export FTP_PROXY=\$ftp_proxy

export no_proxy=.intel.com,intel.com,localhost,127.0.0.1,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16
export NO_PROXY=\$no_proxy

export all_proxy=\$http_proxy
export ALL_PROXY=\$all_proxy
EOF

echo "setup proxy done"
echo "please reboot"
