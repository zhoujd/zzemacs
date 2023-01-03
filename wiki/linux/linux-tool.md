Linux Tool
==========

## Linuxkit

    git clone https://github.com/linuxkit/linuxkit
    cd linuxkit && make
    cp -rf bin/linuxkit /usr/local/linuxkit
    linuxkit build -format iso-bios -name kitiso minimal.yml
    du -sh * | grep linuxkit.iso
    file linuxkit.iso

## chage command in Linux

    ## chage command is used to view and change the user password expiry information
    $ USERADDCMD="useradd -m <user>"
    $ USERCHGCMD="chage -M 99999 sys_cert;passwd -d <user>"
    $ getent passwd 2>/dev/null | grep -m 1 -e <user>
    $ hostnamectl status

## lsar and unar

    $ sudo apt install unar
    $ lsar
    $ unar

## Quick Start for Moby and LinuxKit

    ## https://www.atmosera.com/blog/quick-start-moby-linuxkit/
    ## Need to be root
    $ sudo -i

    ## Now that you are root, create a shell script from the following code.
    $ cat > yourfile.sh <<EOF
    #!/bin/sh
    #Install dependencies for the build
    apt update
    apt install apt-transport-https  ca-certificates curl software-properties-common build-essential git

    # Install Docker
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu  $(lsb_release -cs) stable"
    apt update
    apt install docker-ce

    # Install Go
    wget https://storage.googleapis.com/golang/go1.8.1.linux-amd64.tar.gz
    tar xvf go1.8.1.linux-amd64.tar.gz
    chown -R root:root ./go
    sudo mv go /usr/local
    echo "export GOPATH=$HOME/work" >> ~/.profile
    echo "export PATH=$PATH:/usr/local/go/bin:$GOPATH/bin" >> ~/.profile
    source ~/.profile

    # Install Moby
    go get -u github.com/moby/tool/cmd/moby

    # Install Linux Kit
    git clone https://github.com/linuxkit/linuxkit
    cd ./linuxkit
    make
    make install
    EOF

    ## Once the file is created, save it then run it. This will install all of the dependencies, Docker, Moby and Linux Kit.
    sh /path/to/yourfile.sh

    ## Change directories to the linuxkit directory, which is probably /root/linuxkit, then build the image.
    cd ~/linuxkit

    ## Now, build the image.
    moby build linuxkit.yml

    ## Now, run the image.
    $ linuxkit run linuxkit

    ## Once you’re done playing with it, you can type in halt to exit the image
    $ halt
