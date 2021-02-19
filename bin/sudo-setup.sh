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
SUDO_CFG=${USER_NAME}.conf

cat > /etc/sudoers.d/${SUDO_CFG} <<EOF
## ${SUDO_CFG}
Defaults env_keep+="http_proxy https_proxy ftp_proxy no_proxy socks_proxy all_proxy"
${USER_NAME} ALL=(ALL) ALL
EOF

echo "please reboot"
