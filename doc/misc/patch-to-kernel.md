Patch to kernel
===============

1. Apply to kernel

        bash> cd /usr/src/linux-X.Y.Z/
        bash> bzip2 -dc ../X.Y.Z-mm2.bz2 | patch -p1
        bash> gzip -cd ../patch-3.x.gz | patch -p1

2. Code View:

        bash> diff –Nur /path/to/original/kernel /path/to/your/kernel > changes.patch

3. Build kernel

        bash> cd /usr/src/linux-X.Y.Z/
        bash> make clean
        
        bash> make menuconfig/oldconfig/
        bash> cp arch/x86/configs/i386_defconfig .config
        
        bash> make bzImage
        bash> cp arch/x86/boot/bzImage /boot/vmlinuz

        bash> /sbin/lilo
        bash> reboot

3. Build module

        bash> cd /usr/src/linux-X.Y.Z/
        bash> make modules
        bash> make modules_install

        bash> modprobe vfat
        bash> lsmod

        bash> modinfo vfat

        bash> cd /path/to/module-source/
        bash> echo "obj-m += mymodule.ko" > Makefile
        bash> make –C /path/to/kernel-sources/ M=`pwd` modules

        bash> insmod ./mymodule.ko

4. Linux kernel cross reference

    <http://lxr.oss.org.cn/>

    <http://oss.org.cn/ossdocs/gnu_linux/kernel-api/>

