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

    $ VBoxManage list vms
    $ VBoxManage list bridgedifs

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

## Install VBox Additions

    $ sudo ./VBoxLinuxAdditions.run
    Verifying archive integrity...  100%   MD5 checksums are OK. All good.
    Uncompressing VirtualBox 7.1.6 Guest Additions for Linux  100%
    VirtualBox Guest Additions installer
    VirtualBox Guest Additions: Starting.
    VirtualBox Guest Additions: Setting up modules
    VirtualBox Guest Additions: Building the VirtualBox Guest Additions kernel
    modules.  This may take a while.
    VirtualBox Guest Additions: To build modules for other installed kernels, run
    VirtualBox Guest Additions:   /sbin/rcvboxadd quicksetup <version>
    VirtualBox Guest Additions: or
    VirtualBox Guest Additions:   /sbin/rcvboxadd quicksetup all
    VirtualBox Guest Additions: Building the modules for kernel 6.8.0-52-generic.

    This system is currently not set up to build kernel modules.
    Please install the gcc make perl packages from your distribution.
    VirtualBox Guest Additions: Running kernel modules will not be replaced until
    the system is restarted or 'rcvboxadd reload' triggered
    VirtualBox Guest Additions: reloading kernel modules and services
    VirtualBox Guest Additions: kernel modules were not reloaded
    VirtualBox Guest Additions: kernel modules and services were not reloaded
    The log file /var/log/vboxadd-setup.log may contain further information.
