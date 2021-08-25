#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
MCI_HOME=$(cd $SCRIPT_ROOT/.. && pwd)

ssh_x11() {
    sudo sed -i 's/#AllowTcpForwarding yes/AllowTcpForwarding yes/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11Forwarding yes/X11Forwarding yes/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11DisplayOffset 10/X11DisplayOffset 10/g' /etc/ssh/sshd_config
    sudo sed -i 's/#X11UseLocalhost yes/X11UseLocalhost yes/g' /etc/ssh/sshd_config
}

ssh_root() {
    sudo sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/g' /etc/ssh/sshd_config
}

ssh_restart() {
    sudo systemctl restart sshd
}

ssh_keys() {
    local TARGET="$HOME/.ssh"

    rm -rf $TARGET
    mkdir -p $TARGET

    echo "Install keys to $TARGET/authorized_keys"
    cat $MCI_HOME/misc/ssh/keys/id_rsa_intel.pub >> $TARGET/authorized_keys
}

ssh_keys
ssh_root
ssh_restart 

echo "setup ssh done"
