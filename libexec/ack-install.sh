#!/bin/bash

VER=v3.5.0
#sudo curl https://beyondgrep.com/ack-$VER > /usr/local/bin/ack
sudo wget -O /usr/local/bin/ack https://beyondgrep.com/ack-$VER
sudo chmod +x /usr/local/bin/ack

echo "Install ack to /usr/local/bin/ done"
