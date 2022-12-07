Grub
====

## recover boot sector

    ##First
    sudo grub2-install /dev/sda

    ##Sencond
    sudo fdisk -l
    sudo mount /dev/sda1 /mnt
    sudo grub-install --root-directory=/mnt/ /dev/sda
    sudo reboot
    sudo update-grub
    sudo umount /mnt

    ##Third
    sudo fdisk -l
    sudo mount /dev/sda2 /mnt/boot
    sudo mount --bind /dev /mnt/dev
    sudo chroot /mnt
    sudo update-grub
    sudo grub-install /dev/sda
    sudo umount /mnt/boot
    sudo umount /mnt/dev

## grub2 for freebsd

    menuentry "FreeBSD (on /dev/sda2)" {
        set root=(hd0,msdos2) # base on 'sudo fdisk -l' result
        insmod ufs2
        chainloader +1
    }

    menuentry "Windows (on /dev/sda1)" {
        insmod chain
        insmod ntfs
        set root=(hd0,msdos1)
        chainloader +1
    }

    ## For first extended partition of the first hard disk drive
    (hd1,msdos1,bsd1)

    ==>>
    /etc/grub.d/40_custom***
    rm /boot/grub2/grub.cfg
    sudo grub2-mkconfig -o /boot/grub2/grub.cfg
    sudo update-grub

## grub configfile

    title SLES 11 SP3 (/dev/sda9)
    root (hd0,8)
    configfile /boot/grub/menu.lst

    menuentry "Grub2 config menu" {
        set root='hd0,msdos8'
        configfile /boot/grub/grub.cfg
    }

    menuentry "Fedora Grub2 config menu" {
        configfile (lvm/fedora-root)/grub2/grub.cfg
    }

    menuentry "Grub 1 Bootloader" {
        set root='hd0,msdos8'
        chainloader +1
    }

## grub2 setup

    https://help.ubuntu.com/community/Grub2/Setup#Configuring_GRUB_2
    http://www.dedoimedo.com/computers/grub-2.html

    Device: sudo grub-probe -t device /boot/grub
    UUID: sudo grub-probe -t fs_uuid /boot/grub

    #remove memtest86+
    sudo chmod -x /etc/grub.d/20_memtest86+
    sudo update-grub

    sudo gedit /etc/default/grub
    #GRUB_HIDDEN_TIMEOUT=0                  ##place a "#" symbol at the start of line  for show menu
    #GRUB_TIMEOUT_STYLE=hidden              ##place a "#" symbol at the start of line  for show menu

    GRUB_DISABLE_OS_PROBER=true             ##disable proble os (maybe it is danger)
    GRUB_DEFAULT="1>4"                      ##select default "grep menuentry /boot/grub/grub.cfg"
    #GRUB_DISABLE_LINUX_RECOVERY=true       ##If you want a "Recovery" option for only one kernel, make a special entry in /etc/grub/40_custom.
    GRUB_DISABLE_SUBMENU=y                  ##disable submenu on ubuntu 14.04

## Change or Set Default Kernel Version

    $ sudo nano /etc/default/grub
    GRUB_SAVEDEFAULT=true
    GRUB_DEFAULT=saved

## Cannot get grub menu to timeout (or go away)

    $ sudo nano /etc/default/grub
    GRUB_TIMEOUT=5
    GRUB_RECORDFAIL_TIMEOUT=$GRUB_TIMEOUT

    $ sudo update-grub

## GNU GRUB Menu: Change the Default Boot OS

    $ grep -m1 submenu /boot/grub/grub.cfg | cut -f4 -d "'"
    gnulinux-advanced-594ab82a-3af6-46aa-bdf4-6b49be6818dc

    $ grep menuentry /boot/grub/grub.cfg | cut -f4 -d "'" | grep gnulinux-5.14.0-1033-oem-advanced
    gnulinux-5.14.0-1033-oem-advanced-981282df-93c0-471c-b6b8-4b4df476ad4d

    $ sudo vim /etc/default/grub
    GRUB_DEFAULT=gnulinux-advanced-594ab82a-3af6-46aa-bdf4-6b49be6818dc>gnulinux-5.14.0-1033-oem-advanced-981282df-93c0-471c-b6b8-4b4df476ad4d

    $ NEW=gnulinux-advanced-594ab82a-3af6-46aa-bdf4-6b49be6818dc>gnulinux-5.14.0-1033-oem-advanced-981282df-93c0-471c-b6b8-4b4df476ad4d
    $ sudo sed -i 's/GRUB_DEFAULT=/GRUB_DEFAULT=$NEW/g' /etc/default/grub

    $ sudo update-grub
