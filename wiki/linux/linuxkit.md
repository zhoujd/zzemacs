Linux Kit
=========

## Linuxkit 

    git clone https://github.com/linuxkit/linuxkit
    cd linuxkit && make
    cp -rf bin/linuxkit /usr/local/linuxkit
    linuxkit build -format iso-bios -name kitiso minimal.yml
    du -sh * | grep linuxkit.iso 
    file linuxkit.iso

