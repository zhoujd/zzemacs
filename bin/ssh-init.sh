#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

ssh_x11() {
    echo "ssh x11 config"
    sudo sed -i 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11Forwarding yes/X11Forwarding yes/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11UseLocalhost yes/X11UseLocalhost yes/g' /etc/ssh/sshd_config    
}

ssh_root() {
    echo "ssh root config"
    sudo sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
}

ssh_restart() {
    echo "ssh restart"
    sudo systemctl restart sshd
}

ssh_x11
ssh_root
ssh_restart 

echo "setup ssh done"
