#!/bin/bash
#set -x
        
if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

if [ $# -ne 1 ]; then
   echo "Usage: `basename $0` <user>"
   exit 1
fi

USER_NAME=$1

cat > /etc/sudoers.d/${USER_NAME} <<EOF
## ${USER_NAME}
Defaults env_keep+="http_proxy https_proxy ftp_proxy no_proxy socks_proxy all_proxy"
${USER_NAME} ALL=(ALL) ALL
EOF

echo "please reboot"
