#!/bin/bash
#set -x

## https://github.com/microsoft/vscode-dev-containers/tree/main/containers/docker-from-docker-compose
## https://spin.atomicobject.com/2021/06/16/docker-development-container/

Install_docker() {
    echo "Install Docker CE CLI"
    sudo apt-get update \
         sudo apt-get install -y apt-transport-https ca-certificates curl gnupg2 lsb-release \
         curl -fsSL https://download.docker.com/linux/$(lsb_release -is | tr '[:upper:]' '[:lower:]')/gpg | sudo apt-key add - 2>/dev/null
    echo "deb [arch=$(dpkg --print-architecture)] https://download.docker.com/linux/$(lsb_release -is | tr '[:upper:]' '[:lower:]') $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list
    sudo apt-get update
    sudo apt-get install -y docker-ce-cli
}

Install_compos() {
    echo "Install Docker Compose"
    export LATEST_COMPOSE_VERSION=$(curl -sSL "https://api.github.com/repos/docker/compose/releases/latest" | grep -o -P '(?<="tag_name": ").+(?=")')
    sudo curl -sSL "https://github.com/docker/compose/releases/download/${LATEST_COMPOSE_VERSION}/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose
}

Install_docker
Install_compose

echo "Docker install done"
