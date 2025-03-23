#!/bin/bash

sudo tee /etc/apt/sources.list.d/vs-code.list <<EOF
deb [arch=amd64] http://packages.microsoft.com/repos/vscode stable main
EOF

curl https://packages.microsoft.com/keys/microsoft.asc | \
    sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/microsoft.gpg

sudo apt-get update
sudo apt-get install code

echo "VSCode Install Done"
