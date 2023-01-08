#!/bin/bash

## sudo apt-get install virt-viewer
## virt-manager --connect=<URI> --show-domain-console <domain>
## virt-manager --connect=qemu:///system --show-domain-console <domain>

if [ $# -ne 1 ];then
    echo "Usage: $(basename $0) <domainname>"
    virsh list --all
    exit 1
fi

virsh domstate $1 | grep running 2>&1 1>/dev/null
if [ $? -ne 0 ] ; then
  echo Starting VM $1
  virsh start $1       # domain must be known to virsh

fi

virt-viewer -w $1      # -w to wait until domain is running.
