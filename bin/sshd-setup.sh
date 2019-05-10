#!/bin/bash

## cat /etc/ssh/sshd_config
## AllowTcpForwarding yes
## X11Forwarding yes
## X11DisplayOffset 10
## X11UseLocalhost yes

sudo sed -i 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' /etc/ssh/sshd_config
sudo sed -i 's/#X11Forwarding yes/X11Forwarding yes/g' /etc/ssh/sshd_config
sudo sed -i 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' /etc/ssh/sshd_config
sudo sed -i 's/#X11UseLocalhost yes/X11UseLocalhost yes/g' /etc/ssh/sshd_config

sudo systemctl restart sshd

echo "setup /etc/ssh/sshd_config done"
