Fedora
======

## dnf proxy

    $ cat /etc/dnf/dnf.conf
    proxy=http://host:port/
    fastestmirror=true

## Rpmfusion

    $ sudo dnf install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-30.noarch.rpm
    $ sudo dnf install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-29.noarch.rpm
    $ sudo dnf install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-28.noarch.rpm

## x264 & x265

    $ sudo dnf -y install x264-devel
    $ sudo dnf -y install x265-devel

