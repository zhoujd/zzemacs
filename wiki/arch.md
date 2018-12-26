Arch setting
==============

1. Build kernel from source.
   
        $ wget https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.15.2.tar.xz
        $ tar xvf linux-4.15.2.tar.xz

        $ cd linux-4.15.2
        $ sudo pacman -S ncurses make gcc bc openssl

        $ zcat /proc/config.gz > .config
        $ make olddefconfig
        $ make
        $ sudo make modules_install
        $ sudo cp -v arch/x86_64/boot/bzImage /boot/vmlinuz-4.15.2
        $ sudo mkinitcpio -k 4.15.2-ARCH -g /boot/initramfs-4.15.2.img
        $ sudo cp -v System.map /boot/System.map-4.15.2
        $ sudo ln -sf /boot/System.map-4.15.2 /boot/System.map
        $ sudo grub-mkconfig -o /boot/grub/grub.cfg
        $ sudo reboot
        $ uname -r
