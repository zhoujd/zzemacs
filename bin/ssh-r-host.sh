#!/bin/bash

if [ $# != 1 ]; then
    echo "Usage: $0 <host_ip|host_name>"
    exit 1
fi

ssh-keygen -f $HOME/.ssh/known_hosts -R $1

echo "Remove $1 from $HOME/.ssh/known_hosts done."
