#!/bin/bash

## https://code.visualstudio.com/
## https://code.visualstudio.com/download

sudo apt update
sudo apt install -y curl gpg software-properties-common apt-transport-https

sudo tee /etc/apt/sources.list.d/vs-code.list <<EOF
deb [arch=amd64] http://packages.microsoft.com/repos/vscode stable main
EOF

curl https://packages.microsoft.com/keys/microsoft.asc | \
    sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/microsoft.gpg

sudo apt-get update
sudo apt-get install -y code

echo "VSCode Install Done"
