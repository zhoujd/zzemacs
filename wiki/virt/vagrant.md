Vagrant
=======

## Install vagrant-libvirt plugin in Linux

    ## On Ubuntu
    $ sudo apt install qemu libvirt-daemon-system libvirt-clients libxslt-dev libxml2-dev libvirt-dev zlib1g-dev ruby-dev ruby-libvirt ebtables dnsmasq-base
    ## On CentOS, Fedora
    $ sudo dnf install gcc libvirt libvirt-devel libxml2-devel make ruby-devel
    $ vagrant plugin install vagrant-libvirt
    $ vagrant plugin install vagrant-mutate
    $ vagrant init centos/7
    $ vagrant up --provider=libvirt
    $ export VAGRANT_DEFAULT_PROVIDER=libvirt
    $ virsh list
    $ vagrant status
    $ vagrant destroy
    $ vagrant up --provider libvirt


## Use bundler to execute Vagrant

    ## https://github.com/vagrant-libvirt/vagrant-libvirt#libvirt-configuration
    $ sudo apt install ruby-bundler
    $ bundle exec vagrant up --provider=libvirt
