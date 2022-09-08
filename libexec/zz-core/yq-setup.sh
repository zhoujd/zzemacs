#!/bin/bash

VER=v4.6.3
sudo wget -O /usr/local/bin/yq https://github.com/mikefarah/yq/releases/download/$VER/yq_linux_amd64
sudo chmod +x /usr/local/bin/yq

echo "Install yq to /usr/local/bin/ done"
