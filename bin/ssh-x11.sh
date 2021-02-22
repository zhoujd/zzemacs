#!/bin/bash

ssh_x11() {
    echo "Enable x11 forward"
    sudo sed -i 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11Forwarding yes/X11Forwarding yes/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11UseLocalhost yes/X11UseLocalhost yes/g' /etc/ssh/sshd_config
}

ssh_root() {
    echo "Enable root login"
    sudo sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
}

ssh_restart() {
    echo "Restart sshd"
    sudo systemctl restart sshd
}

ssh_x11
ssh_restart

echo "ssh-x11 all done"
