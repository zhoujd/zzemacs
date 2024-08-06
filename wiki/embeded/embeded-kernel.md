Kernel and rootfs build for QEMU
================================

## Prepare

```
#!/bin/bash

mkdir -p ~/dev/i386
cd ~/dev/i386
```

## Vanilla

```
mkdir vanilla
cd vanilla
```

## Crosstool

```
wget https://github.com/crosstool-ng/crosstool-ng/archive/crosstool-ng-1.23.0.tar.gz
tar xzf crosstool-ng-1.23.0.tar.gz
cd crosstool-ng-crosstool-ng-1.23.0
./bootstrap
./configure --enable-local
make
test -x ct-ng || echo "ctng setup unsuccessful"
./ct-ng x86_64-unknown-linux-gnu
./ct-ng menuconfig # Change #of parallel jobs two 4 and remove fortran and java languages
./ct-ng build
export PATH=$PATH:$HOME/x-tools/x86_64-unknown-linux-gnu/bin/
(test -x x86_64-unknown-linux-gnu-gcc && x86_64-unknown-linux-gnu-gcc -v) || echo "ctng build unsuccessful"
cd ..
```

## Linux kernel

```
sudo apt install libelf-dev
wget https://mirrors.edge.kernel.org/pub/linux/kernel/v4.x/linux-4.17.tar.gz
tar xzf linux-4.17.tar.gz
cd linux-4.17/
make ARCH=x86 x86_64_defconfig
make ARCH=x86 menuconfig
make ARCH=x86 CROSS_COMPILE=x86_64-unknown-linux-gnu- all
test -f ./arch/x86/boot/bzImage || echo "kernel build unsuccessful"
cd ..
```

## QEMU

```
wget https://download.qemu.org/qemu-2.12.0.tar.bz2
tar xjf qemu-2.12.0.tar.bz2
cd qemu-2.12.0/
mkdir build
cd build/
../configure --target-list="i386-softmmu x86_64-softmmu"
make
test -f ./x86_64-softmmu/qemu-system-x86_64 || echo "qemu build unsuccessful"
cd ../..
```

## Testing program (GNU Hello!)

```
wget http://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz
tar xzf hello-2.10.tar.gz
cd hello-2.10/
CC=x86_64-unknown-linux-gnu-gcc ./configure --host=x86_64-unknown-linux-gnu
make
test -f ./hello || echo "Hello build unsuccessful"
cd ..
```

## Busybox

```
wget https://busybox.net/downloads/busybox-1.28.0.tar.bz2
tar xjf busybox-1.28.0.tar.bz2
cd busybox-1.28.0/
make ARCH=x86_64 CROSS_COMPILE=x86_64-unknown-linux-gnu- defconfig
make ARCH=x86_64 CROSS_COMPILE=x86_64-unknown-linux-gnu- menuconfig # Select "Build static binary"
make ARCH=x86_64 CROSS_COMPILE=x86_64-unknown-linux-gnu- install
test -d _install || echo "Busybox build unsuccessful"
```

## Build the rootfs

```
CROSSTOOLNG_DIR=$HOME/x-tools/x86_64-unknown-linux-gnu/x86_64-unknown-linux-gnu/
cd _install/
cp ../../hello-2.10/hello usr/bin/.
mkdir -p lib lib64 proc sys etc etc/init.d
cp $CROSSTOOLNG_DIR/sysroot/lib/*so* lib/.
cp $CROSSTOOLNG_DIR/sysroot/lib64/*so* lib64/.
```

## Configure and assemble the rootfs

```
sudo busybox dumpkmap > etc/host.kmap
cat > ./etc/init.d/rcS << EOF
#!/bin/sh

# Mount the /proc and /sys filesystems
mount -t proc none /proc
mount -t sysfs none /sys

# Populate /dev
/sbin/mdev -s

# Enable the localhost interface
ifconfig lo up

# Manually configure the eth0 interface. Note that QEMU has a built-in
# DHCP server that assigns addresses to the hosts starting from 10.0.2.15.
ifconfig eth0 10.0.2.15 netmask 255.255.255.0
route add default gw 10.0.2.1

# Comment this line if changing your keyboard layout is not needed
loadkmap < /etc/host.kmap
EOF

chmod +x etc/init.d/rcS
find . | cpio -o --format=newc | gzip > ../../rootfs.img.gz
cd ../..
```

## Finally, run the app (exits with CTRL-A then X)

```
./qemu-2.12.0/build/x86_64-softmmu/qemu-system-x86_64 \
  -M pc \
  -m 1024M \
  -kernel ./linux-4.17/arch/x86/boot/bzImage \
  -initrd rootfs.img.gz \
  -append "root=/dev/ram rdinit=/sbin/init console=ttyS0 nokaslr" \
  -nographic \
  -serial mon:stdio
```
