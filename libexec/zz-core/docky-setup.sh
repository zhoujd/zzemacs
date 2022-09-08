#!/bin/bash

## https://askubuntu.com/questions/1237396/install-docky-in-ubuntu-20-04
## https://packages.ubuntu.com/bionic/docky

mkdir -p ~/Downloads/docky
cd ~/Downloads/docky

install_deps() {
    echo "Install deps"
    wget http://archive.ubuntu.com/ubuntu/pool/universe/g/gnome-sharp2/libgconf2.0-cil_2.24.2-4_all.deb
    wget http://archive.ubuntu.com/ubuntu/pool/main/g/glibc/multiarch-support_2.27-3ubuntu1_amd64.deb
    wget http://archive.ubuntu.com/ubuntu/pool/universe/libg/libgnome-keyring/libgnome-keyring-common_3.12.0-1build1_all.deb
    wget http://archive.ubuntu.com/ubuntu/pool/universe/libg/libgnome-keyring/libgnome-keyring0_3.12.0-1build1_amd64.deb
    wget http://archive.ubuntu.com/ubuntu/pool/universe/g/gnome-keyring-sharp/libgnome-keyring1.0-cil_1.0.0-5_amd64.deb
    sudo apt-get install ./*.deb
}

install_docky() {
    echo "Install docky"
    wget http://archive.ubuntu.com/ubuntu/pool/universe/d/docky/docky_2.2.1.1-1_all.deb
    sudo apt-get install ./docky_2.2.1.1-1_all.deb
}

install_cfg() {
    echo "Install docky cfg"
    gconftool-2 --type Boolean --set /apps/docky-2/Docky/Items/DockyItem/ShowDockyItem False
}

install_deps
install_docky
insetal_cfg

echo "Install docky done"
