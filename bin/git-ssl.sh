#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
MISC_ROOT=$(cd $SCRIPT_ROOT/../misc && pwd)

install_ca_org() {
    echo "install cacert.org"
    local ca_cert=/usr/local/share/ca-certificates/cacert.org
    sudo rm -rf $ca_cert
    sudo mkdir -p $ca_cert
    sudo wget -P $ca_cert \
         http://www.cacert.org/certs/root.crt \
         http://www.cacert.org/certs/class3.crt
}

install_ca_git() {
    echo "install bundle"
    local ca_git=/usr/local/share/ca-certificates/git-certs
    sudo rm -rf $ca_git
    sudo mkdir -p $ca_git
    sudo cp $MISC_ROOT/ca-bundle.crt $ca_git
}

update_ca() {
    echo "ssl update ca"
    sudo update-ca-certificates

    echo "git ssl update"
    git config --global http.sslverify true
    git config --global http.sslCAinfo /etc/ssl/certs/ca-certificates.crt
}

install_ca_org
install_ca_git
update_ca

echo "ssl update done"
