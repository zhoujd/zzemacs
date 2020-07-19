#!/bin/bash


install_ca_org() {
    echo "install cacert.org"
    local ca_target=/usr/local/share/ca-certificates/cacert.org
    sudo rm -rf $ca_target
    sudo mkdir $ca_target
    sudo wget -P $ca_target \
         http://www.cacert.org/certs/root.crt \
         http://www.cacert.org/certs/class3.crt
}

install_ca_bundle() {
    echo "install bundle"
    local ca_bundle=/usr/local/share/ca-certificates/cabundle
    sudo rm -rf $ca_bundle
    sudo mkdir $ca_bundle
    sudo cp ~/zzemacs/misc/ca-bundle.crt $ca_bundle
}

update_ca() {
    echo "ssl update ca"
    sudo update-ca-certificates

    echo "git ssl update"
    git config --global http.sslCAinfo /etc/ssl/certs/ca-certificates.crt
}

install_ca_org
install_ca_bundle
update_ca

echo "ssl update done"
