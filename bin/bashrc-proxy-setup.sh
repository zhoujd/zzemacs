#!/bin/sh

cat > $HOME/.bash_proxy <<EOF
## This is for bash proxy
export http_proxy=$1
export https_proxy=\$http_proxy
export ftp_proxy=\$http_proxy
EOF

echo "Setup proxy .bashrc end ..."
