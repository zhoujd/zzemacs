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

    ## 1. Find the string for the OS you want to set as the default.
    $ grep menuentry /boot/grub/grub.cfg

    ## 2. Highlight the string and copy it to the clipboard. (include the quotes.)
    e.g. "Windows 10 (loader) (on /dev/sda1)"

    ## 3. Edit /etc/default/grub
    $ sudo vi /etc/default/grub

    ## 4. Change the value of GRUB_DEFAULT from 0 to the OS string you copied from /boot/grub/grub.cfg, then save /etc/default/grub.

    ## 5. Regenerate the Grub menu, /boot/grub/grub.cfg, by running the following command:
    $ sudo update-grub

    ## 6. Reboot the computer to verify the expected OS is the default.
