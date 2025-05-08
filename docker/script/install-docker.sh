#!/bin/bash

. /etc/os-release

## Run as root
if [ $EUID -ne 0 ]; then
    echo "You must be a root user" 2>&1
    exit 1
fi

ubuntu() {
    apt-get update \
        && apt-get install ca-certificates curl \
        && mkdir -p /etc/apt/keyrings \
        && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" \
        | tee /etc/apt/sources.list.d/docker.list
    apt-get update \
        && apt-get install -y --no-install-recommends \
                   docker-ce docker-ce-cli containerd.io docker-compose-plugin \
        && apt-get autoremove \
        && apt-get clean
    echo "Install Docker on Ubuntu Done"
}

debian() {
    apt-get update \
        && apt-get install ca-certificates curl \
        && install -m 0755 -d /etc/apt/keyrings \
        && curl -fsSL https://download.docker.com/linux/debian/gpg -o /etc/apt/keyrings/docker.asc \
        && chmod a+r /etc/apt/keyrings/docker.asc
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/debian $(. /etc/os-release && echo "$VERSION_CODENAME") stable" \
        | tee /etc/apt/sources.list.d/docker.list
    apt-get update \
        && apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin \
        && apt-get autoremove \
        && apt-get clean
    echo "Install Docker on Debian Done"
}

case $ID in
    debian )
        echo "Install on $ID"
        debian
        ;;
    ubuntu )
        echo "Install on $ID"
        ubuntu
        ;;
    * )
        echo "Install on $ID is not supported"
        ;;
esac
