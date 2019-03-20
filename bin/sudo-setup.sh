#!/bin/bash
#set -x
        
if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

cat > /etc/sudoers.d/${USER} <<EOF
## ${USER}
Defaults env_keep+="http_proxy https_proxy ftp_proxy no_proxy"
${USER} ALL=(ALL) ALL
EOF

echo "please reboot"
