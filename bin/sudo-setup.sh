#!/bin/bash
#set -x
        
if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

USER_NAME=zhoujd

cat > /etc/sudoers.d/${USER_NAME} <<EOF
## ${USER_NAME}
Defaults env_keep+="http_proxy https_proxy ftp_proxy no_proxy socks_proxy"
%wheel ALL=(ALL) ALL
EOF

echo "please reboot"
