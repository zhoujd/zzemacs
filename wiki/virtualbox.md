VirtualBox
==========

1. Share folder with auto mount (/media/sf_win10)
   
        $ sudo usermod -a -G vboxsf zhoujd
        or
        $ sudo chmod 777 /media/sf_win10


2. Command line

        $ vboxmanage list vms
        

3. Enable kernel support

        $ sudo pacman -S virtualbox-host-dkms
        $ sudo modprobe vboxdrv

4. On Arch Linux

        ## https://wiki.archlinux.org/index.php/VirtualBox
        ## http://www.cs.columbia.edu/~jae/4118/arch-setup-2018-1.html

        $ sudo systemctl enable vboxservice.service
        $ sudo systemctl start vboxservice.service

        $ sudo pacman -S virtualbox-guest-utils virtualbox-host-dkms
        $ VBoxClient --clipboard --draganddrop --seamless --display --checkhostversion
        $ VBoxClient-all
