Busybox
=======

## Build Busybox

    ## https://www.centennialsoftwaresolutions.com/post/build-the-linux-kernel-and-busybox-and-run-them-on-qemu
    ## Steps
    ## 1. Open a terminal

    ## 2. Get the required packages
    $ sudo apt install curl libncurses5-dev qemu-system-x86

    ## 3. Create a workspace
    $ STAGE=~/tl
    $ TOP=$STAGE/teeny-linux
    $ mkdir -p $STAGE

    ## 4. Download and extract the BusyBox
    $ cd $STAGE
    $ git clone https://git.busybox.net/busybox

    ## 5. Create a minimal userland with Busybox
    $ cd $STAGE/busybox
    $ make defconfig

    ## 6. Enable static linking in Busybox
    ## Busybox Settings ---> Build Options ---> Build BusyBox as a static binary (no shared libs) ---> yes
    ## sed -i '/# CONFIG_STATIC is not set/c\CONFIG_STATIC=y' ../obj/busybox-x86/.config
    ## In .config
    ## CONFIG_STATIC=y
    ## CONFIG_STATIC_LIBGCC=y
    $ make menuconfig

    ## 7. Build Busybox
    $ make -j4
    $ make install

    ## 8. Build the directory structure of the initramfs
    $ mkdir -pv $TOP/initramfs/x86-busybox
    $ cd $TOP/initramfs/x86-busybox
    $ mkdir -pv {bin,dev,sbin,etc,proc,sys/kernel/debug,usr/{bin,sbin},lib,lib64,mnt/root,root}
    $ cp -av $STAGE/busybox/_install/* $TOP/initramfs/x86-busybox
    $ sudo cp -av /dev/{null,console,tty,sda1} $TOP/initramfs/x86-busybox/dev/

    ## 9. Create init and make it executable
    ## 9.1 Type:
    $ vi $TOP/initramfs/x86-busybox/init
    ## 9.2 Paste this in (press i)
    #!/bin/sh

    mount -t proc none /proc
    mount -t sysfs none /sys
    mount -t debugfs none /sys/kernel/debug

    echo -e "\nBoot took $(cut -d' ' -f1 /proc/uptime) seconds\n"

    exec /bin/sh
    ## 9.3 Type :w to save, then :q to quit

    ## 10. Make init executable
    $ chmod +x $TOP/initramfs/x86-busybox/init

    ## 11. Create the initramfs
    $ cd $TOP/initramfs/x86-busybox
    $ find . | cpio -H newc -o > ../initramfs.cpio
    $ cd ..
    $ mkdir -p $TOP/obj
    $ cat initramfs.cpio | gzip > $TOP/obj/initramfs.igz

## Build and run minimal Linux

    ## https://gist.github.com/chrisdone/02e165a0004be33734ac2334f215380e
    ## Add the -enable-kvm option if your host has KVM enabled
    $ qemu-system-x86_64 -kernel $LINUX_BUILD/arch/x86_64/boot/bzImage \
        -initrd $BUILDROOT_BUILD/images/rootfs.cpio.gz -nographic \
        -append "console=ttyS0"
