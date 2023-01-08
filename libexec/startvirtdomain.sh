#!/bin/bash

## sudo apt-get install virt-viewer
## virt-manager --connect=<URI> --show-domain-console <domain>
## virt-manager --connect=qemu:///system --show-domain-console <domain>

if [ $# -ne 1 ];then
    echo "Usage: $(basename $0) <domainname>"
    virsh list --all
    exit 1
fi

VM=$1
VIEWER=virt-manager

virsh domstate $VM | grep running 2>&1 1>/dev/null
if [ $? -ne 0 ] ; then
    echo Starting VM $VM
    virsh start $VM       # domain must be known to virsh
fi

case $VIEWER in
    virt-manager )
        virsh domstate $VM | grep running 2>&1 1>/dev/null
        while [ $? -ne 0 ]; do
            sleep 3
            virsh domstate $VM | grep running 2>&1 1>/dev/null
        done
        virt-manager --connect=qemu:///system --show-domain-console $VM
        ;;
    virt-viewer )
        virt-viewer -w $VM      # -w to wait until domain is running.
        ;;
    * )
        echo "Unknown VIEWER setting $VIEWER"
        exit 1
        ;;
esac
