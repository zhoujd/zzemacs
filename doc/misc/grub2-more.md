Grub2 more
===================

1. recover boot sector

        ##First
        sudo grub2-install /dev/sda

        ##Sencond
        sudo fdisk -l
        sudo mount /dev/sda1 /mnt
        sudo grub-install --root-directory=/mnt/ /dev/sda
        sudo reboot
        sudo update-grub

        ##Third
        sudo fdisk -l
        sudo mount /dev/sda2 /mnt/boot
        sudo mount --bind /dev /mnt/dev
        sudo chroot /mnt
        sudo update-grub
        sudo grub-install /dev/sda

2. grub2 for freebsd

        menuentry "FreeBSD(on /dev/sda2)" {
            set root=(hd0,msdos2) # base on 'sudo fdisk -l' result 
            insmod ufs2
            chainloader +1
        }

        ==>>
        /etc/grub.d/40_custom***
        rm /boot/grub2/grub.cfg
        sudo grub2-mkconfig -o /boot/grub2/grub.cfg
        sudo update-grub

3. grub configfile

        title SLES 11 SP3 (/dev/sda9)
	    root (hd0,8)
	    configfile /boot/grub/menu.lst

        menuentry "Grub2 config menu" {
            set root='hd0,msdos8'
            configfile /boot/grub/grub.cfg
        }

        menuentry "Grub 1 Bootloader" {
            set root='hd0,msdos8'
            chainloader +1
        }

4. grub2 setup
        https://help.ubuntu.com/community/Grub2/Setup#Configuring_GRUB_2

        Device: sudo grub-probe -t device /boot/grub
        UUID: sudo grub-probe -t fs_uuid /boot/grub

        #remove memtest86+
        sudo chmod -x /etc/grub.d/20_memtest86+
        sudo update-grub

        sudo gedit /etc/default/grub
        #GRUB_HIDDEN_TIMEOUT=0                  ##uncomment for show menu
        GRUB_DISABLE_OS_PROBER=true             ##disable proble os (maybe it is danger)
        GRUB_DEFAULT="1>4"                      ##select default "grep menuentry /boot/grub/grub.cfg"
        #GRUB_DISABLE_LINUX_RECOVERY=true       ##If you want a "Recovery" option for only one kernel, make a special entry in /etc/grub/40_custom.

