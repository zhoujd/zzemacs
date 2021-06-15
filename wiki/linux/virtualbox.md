VirtualBox
==========

## Share folder with auto mount (/media/sf_win10)

    $ sudo usermod -a -G vboxsf zhoujd
    or
    $ sudo chmod 777 /media/sf_win10

    ## mount manually
    $ sudo mkdir -p /mnt/VMshare
    $ sudo mount -t vboxsf VMshare /mnt/VMshare

## Command line

    $ vboxmanage list vms

## Enable kernel support

    $ sudo pacman -S virtualbox-host-dkms
    $ sudo modprobe vboxdrv

## On Arch Linux

    ## https://wiki.archlinux.org/index.php/VirtualBox
    ## http://www.cs.columbia.edu/~jae/4118/arch-setup-2018-1.html

    $ sudo systemctl enable vboxservice.service
    $ sudo systemctl start vboxservice.service

    $ sudo pacman -S virtualbox-guest-utils virtualbox-host-dkms
    $ VBoxClient --clipboard --draganddrop --seamless --display --checkhostversion
    $ VBoxClient-all

## VM in virtualbox is already locked for a session (or being unlocked)

    $ vboxmanage startvm <vm-uuid> --type emergencystop
