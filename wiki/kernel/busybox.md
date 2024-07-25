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
    $ git clone -b 1_36_stable https://git.busybox.net/busybox

    ## 5. Create a minimal userland with Busybox
    $ cd $STAGE/busybox
    $ make O=$TOP/obj/busybox-x86 defconfig

    ## 6. Enable static linking in Busybox
    ## Busybox Settings ---> Build Options ---> Build BusyBox as a static binary (no shared libs) ---> yes
    ## sed -i '/# CONFIG_STATIC is not set/c\CONFIG_STATIC=y' ../obj/busybox-x86/.config
    ## In .config
    ## CONFIG_STATIC=y
    ## CONFIG_STATIC_LIBGCC=y
    $ cd $TOP/obj/busybox-x86
    $ make menuconfig

    ## 7. Build Busybox
    $ make -j4
    $ make install

    ## 8. Build the directory structure of the initramfs
    $ mkdir -pv $TOP/initramfs/x86-busybox
    $ cd $TOP/initramfs/x86-busybox
    $ mkdir -pv {bin,dev,sbin,etc,proc,sys/kernel/debug,usr/{bin,sbin},lib,lib64,mnt/root,root}
    $ cp -av $TOP/obj/busybox-x86/_install/* $TOP/initramfs/x86-busybox
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
    ## Common
    export OPT=/opt
    export BUILDS=/some/where/mini_linux
    mkdir -p $BUILDS

    ## Build Kernel
    $ export LINUX=$OPT/linux
    $ export LINUX_BUILD=$BUILDS/linux
    $ mkdir -p $LINUX_BUILD
    $ cd $LINUX
    $ make O=$LINUX_BUILD allnoconfig
    $ cd $LINUX_BUILD
    $ make menuconfig
    64-bit kernel ---> yes
    General setup ---> Initial RAM filesystem and RAM disk (initramfs/initrd) support ---> yes
    General setup ---> Configure standard kernel features ---> Enable support for printk ---> yes
    Executable file formats / Emulations ---> Kernel support for ELF binaries ---> yes
    Executable file formats / Emulations ---> Kernel support for scripts starting with #! ---> yes
    Device Drivers ---> Generic Driver Options ---> Maintain a devtmpfs filesystem to mount at /dev ---> yes
    Device Drivers ---> Generic Driver Options ---> Automount devtmpfs at /dev, after the kernel mounted the rootfs ---> yes
    Device Drivers ---> Character devices ---> Enable TTY ---> yes
    Device Drivers ---> Character devices ---> Serial drivers ---> 8250/16550 and compatible serial support ---> yes
    Device Drivers ---> Character devices ---> Serial drivers ---> Console on 8250/16550 and compatible serial port ---> yes
    File systems ---> Pseudo filesystems ---> /proc file system support ---> yes
    File systems ---> Pseudo filesystems ---> sysfs file system support ---> yes
    $ make -j8
    Kernel: arch/x86/boot/bzImage is ready  (#1)

    ## Build Busybox
    $ export BUSYBOX=$OPT/busybox
    $ export BUSYBOX_BUILD=$BUILDS/busybox
    $ mkdir -p $BUSYBOX_BUILD
    $ cd $BUSYBOX
    $ make O=$BUSYBOX_BUILD defconfig
    $ cd $BUSYBOX_BUILD
    $ make menuconfig
    ## Busybox Settings ---> Build Options ---> Build BusyBox as a static binary (no shared libs) ---> yes
    $ make -j4
    $ make install

    ## Add the -enable-kvm option if your host has KVM enabled
    $ qemu-system-x86_64 -kernel $LINUX_BUILD/arch/x86_64/boot/bzImage \
        -initrd $BUILDROOT_BUILD/images/rootfs.cpio.gz -nographic \
        -append "console=ttyS0"

## Buildroot

    $ export BUILDROOT=$OPT/buildroot
    $ export BUILDROOT_BUILD=$BUILDS/buildroot
    $ mkdir -p $BUILDROOT_BUILD
    $ cd $BUILDROOT_BUILD
    $ touch Config.in external.mk
    $ echo 'name: mini_linux' > external.desc
    $ echo 'desc: minimal linux system with buildroot' >> external.desc
    $ mkdir configs overlay
    $ cd $BUILDROOT
    $ make O=$BUILDROOT_BUILD BR2_EXTERNAL=$BUILDROOT_BUILD qemu_x86_64_defconfig
    $ cd $BUILDROOT_BUILD
    $ make menuconfig
    Build options ---> Location to save buildroot config ---> $(BR2_EXTERNAL)/configs/mini_linux_defconfig
    Build options ---> Download dir ---> /some/where/buildroot_dl
    Build options ---> Number of jobs to run simultaneously (0 for auto) ---> 8
    Build options ---> Enable compiler cache ---> yes
    Build options ---> Compiler cache location ---> /some/where/buildroot_ccache
    Toolchain ---> Toolchain type ---> External toolchain
    Toolchain ---> Toolchain ---> Custom toolchain
    Toolchain ---> Toolchain origin ---> Pre-installed toolchain
    Toolchain ---> Toolchain path ---> /opt/toolchains/x86_64-unknown-linux-gnu
    Toolchain ---> Toolchain prefix ---> x86_64-unknown-linux-gnu
    Toolchain ---> External toolchain gcc version ---> 5.x
    Toolchain ---> External toolchain kernel headers series ---> 4.3.x
    Toolchain ---> External toolchain C library ---> glibc/eglibc
    Toolchain ---> Toolchain has C++ support? ---> yes
    System configuration ---> System hostname ---> mini_linux
    System configuration ---> System banner ---> Welcome to mini_linux
    System configuration ---> Run a getty (login prompt) after boot ---> TTY port ---> ttyS0
    System configuration ---> Network interface to configure through DHCP --->
    System configuration ---> Root filesystem overlay directories ---> $(BR2_EXTERNAL)/overlay
    Kernel ---> Linux Kernel ---> no
    Filesystem images ---> cpio the root filesystem (for use as an initial RAM filesystem) ---> yes
    Filesystem images ---> Compression method ---> gzip

    $ make savedefconfig

    ## Add overlay/init
    $ chmod +x overlay/init

    # make

## Add and run a custom user application

    $ export APPS=$BUILDS/apps
    $ mkdir -p $APPS
    $ cd $APPS
    $ make
    $ cp hello_world $BUILDROOT_BUILD/overlay

    $ cd $BUILDROOT_BUILD
    $ make

    $ qemu-system-x86_64 -kernel $LINUX_BUILD/arch/x86_64/boot/bzImage \
        -initrd $BUILDROOT_BUILD/images/rootfs.cpio.gz -nographic \
        -append "console=ttyS0"

## Add loadable module support to the Linux kernel

    $ cd $LINUX_BUILD
    $ make menuconfig
    Enable loadable module support ---> yes

    $ make -j8
    $ make -j8 modules
    $ make modules_install INSTALL_MOD_PATH=$BUILDROOT_BUILD/overlay

    $ cd $BUILDROOT_BUILD
    $ make

    ## The parameter -s is -gdb tcp::1234, it listens on 1234
    ## GDB can use 'target remote localhost:1234' to connect
    $ qemu-system-x86_64 -s -kernel $LINUX_BUILD/arch/x86_64/boot/bzImage \
        -initrd $BUILDROOT_BUILD/images/rootfs.cpio.gz -nographic \
        -append "console=ttyS0"
