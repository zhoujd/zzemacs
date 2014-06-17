Grub2 more
===================

1. recover boot sector

    sudo grub2-install /dev/sda

2. grub2 for freebsd

        menuentry "FreeBSD(on /dev/sda2)" {
            set root=(hd0, msdos2) # base on 'sudo fdisk -l' result 
            insmod ufs2
            chainloader +1
        }

        ==>>
        /etc/grub.d/40_custom***
        rm /boot/grub2/grub.cfg
        sudo grub2-mkconfig > /boot/grub2/grub.cfg
        sudo update-grub

