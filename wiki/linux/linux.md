Linux something
================

## rpm package

    <http://linux.vbird.org/linux_basic/0520rpm_and_srpm.php>

    $ ls *rpm | egrep -v "samples" | xargs sudo yum -y localinstall
    $ sudo yum -y groupinstall "development tools"
    $ sudo yum -y localinstall *.rpm

    $ yum list installed | grep kmd

## linux cross reference

    <http://lxr.oss.org.cn/source/>
    <http://lxr.free-electrons.com/>
    <http://lxr.linux.no/>

## FreeBSD & Linux cross refernce

    <http://fxr.watson.org/>
    <http://svnweb.freebsd.org/>

## Android cross reference

    <http://androidxref.com/>
    <http://code.metager.de/source/xref/android/>

## QEMU

    [QEMU for windows]<http://qemu.weilnetz.de/>
    [QEMU wiki]<http://wiki.qemu.org/Main_Page>

    sudo apt-get install kvm qemu qemu-kvm virt-manager kernel-package linux-source kqemu-source build-essential
    yum install kvm kmod-kvm qemu
    modprobe kvm-intel or modprobe kvm-amd
    /sbin/lsmod | grep kvm
    #yum provides "*/qemu-kvm"
    sudo qemu-img create –f qcow windows.img 8G
    sudo kvm -localtime -cdrom /dev/cdrom -m 512 -boot d win2.img
    sudo kvm -localtime -m 512 -hda windows.img -cdrom winxp.iso -boot d -clock -rtc -no-acpi

## Multi GCC

    gcc -v
    ls -l /usr/bin/gcc* /usr/bin/g++*
    sudo apt-get install gcc-4.7 gcc-4.7-multilib g++-4.7 g++-4.7-multilib

    sudo update-alternatives --remove gcc /usr/bin/gcc-4.6
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.7 60 --slave /usr/bin/g++ g++ /usr/bin/g++-4.7
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 40 --slave /usr/bin/g++ g++ /usr/bin/g++-4.6

    sudo update-alternatives --config gcc

## CentOS rpm

    ## http://wiki.centos.org/HowTos/RebuildSRPM
    ## http://wiki.centos.org/HowTos/I_need_the_Kernel_Source
    sudo yum update kernel-2.6.32-431.17.1.el6
    sudo yum install kernel-devel
    mkdir -p ~/rpmbuild/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS}
    echo '%_topdir %(echo $HOME)/rpmbuild' > ~/.rpmmacros
    sudo yum install rpm-build redhat-rpm-config asciidoc hmaccalc perl-ExtUtils-Embed xmlto
    sudo yum install audit-libs-devel binutils-devel elfutils-devel elfutils-libelf-devel
    sudo yum install newt-devel python-devel zlib-devel
    sudo yum install bison patchutils gcc ncurses-devel
    rpm -i http://vault.centos.org/6.5/updates/Source/SPackages/kernel-2.6.32-431.17.1.el6.src.rpm 2>&1 | grep -v mock
    cd ~/rpmbuild/SPECS
    rpmbuild [-bp|-ba|-bc] --target=$(uname -m) kernel.spec

    cp /boot/config-`uname -r` .config
    make oldconfig
    make menuconfig
    make prepare
    make modules_prepare
    make M=drivers/gpu/drm

    ##list rpm files
    rpm -qlp *.rpm            ## for uninstalled package
    rpm -ql  package-name     ## for installed package

    ##query file belong rpm package
    rpm -qf /path/filename

## Centos LIB dir when compile

    ./configure --prefix=/usr  --libdir=/usr/lib64

## Enter text console

    SUSE/CentOS: vim /etc/inittab  5=>3
    Ubuntu: vim /etc/default/grub
            GRUB_CMDLINE_LINUX_DEFAULT="quiet splash text"
            sudo update-grub

## Q/A web for Linux/FreeBSD/Unix like
    http://stackexchange.com/

## CD/DVD image burning

    mkisofs -r -o sample.iso my_private

    mkdir /mnt/cdrom
    lsblk -f
    mount /dev/sr0 /mnt/cdrom

    mount -o loop /CentOS-6.9-x86_64-bin-DVD1.iso /mnt/cdrom

    cat /etc/fstab
    /CentOS-bin.DVD1.iso  /mnt/cdrom  iso9660  loop,defaults  0   0


    cdrecord -scanbus ==> you well get dev=*,*,*
    cdrecord -v speed=8 dev=0,0,0 -data cd_image.iso
    cdrecord -v -eject dev=0,0,0 -data cd_image.iso

## CentOS6.5 upgrade gcc 4.7.2

    wget http://people.centos.org/tru/devtools-1.1/devtools-1.1.repo
    sudo cp devtools-1.1.repo /etc/yum.repos.d/devtools-1.1.repo
    sudo yum clean all
    sudo yum install devtoolset-1.1   ==>devtools will be installed under /opt/centos/devtoolset-1.1
    scl enable devtoolset-1.1 bash
    gcc –v

    ##Alternatively, you can explicitly define the following variables to use devtools.
    export CC=/opt/centos/devtoolset-1.1/root/usr/bin/gcc
    export CPP=/opt/centos/devtoolset-1.1/root/usr/bin/cpp
    export CXX=/opt/centos/devtoolset-1.1/root/usr/bin/c++

## Ubuntu proxy

    ##Temp in shell, ~/.bashrc for /etc/profile
    $ export http_proxy="http://hostname:port"
    $ export http_proxy="https://hostname:port"
    $ export ftp_proxy="http://hostname:port"
    $ sudo apt-get update

    ##apt-get
    $ sudo nano /etc/apt/apt.conf

    ##add following
    Acquire::http::proxy "http://hostname:port";
    Acquire::https::proxy "http://hostname:port";
    Acquire::ftp::proxy "http://hostname:port";

## Ubuntu ISO image
    http://releases.ubuntu.com/12.04.5/
    http://cdimage.ubuntu.com/releases/
    http://mirrors.163.com/ubuntu-releases/

## dd backup disk

    ## Disk
    # dd if=/dev/sda of=sda.img bs=4M
    # dd if=sda.img of=/dev/sda

    # dd if=/dev/sda bs=1M | gzip -c > sda.img.gz
    # gzip -cd sda.img.gz | dd of=/dev/sda

    # dd if=/dev/sda bs=1M | bzip2 > sda.img.bz2
    # bzip2 -dc sda.img.bz2 | dd of=/dev/sda

    ## Partition
    # dd if=/dev/sda1 of=sda1.img bs=4M
    # dd if=sda1.img of=/dev/sda1

    # dd if=/dev/sda1 | gzip -c > sda1.img.gz
    # gzip -cd sda1.img.gz | dd of=/dev/sda1

    # dd if=/dev/sda1 | bzip2 > sda1.img.bz2
    # bzip2 -dc sda1.img.bz2 | dd of=/dev/sda1

    ## Files system
    # e2fsck -f /dev/sda1
    # resize2fs /dev/sda1
    # e2fsck -f /dev/sda1

    ## Remote with ssh
    # dd if=/dev/hda? | gzip -c | ssh user@other-machine "cat >/path/to/save/to/filename"
    # cat /path/to/filename | ssh user@knoppix-machine "gunzip -c | dd of=/dev/hda?"

## SUSE ssh access

    # vim /etc/sysconfig/SuSEfirewall2
    ## Make sure
    FW_SERVICES_EXT_TCP="ssh"

    ## YAST firewall can be close by GUI

    # vim /etc/ssh/sshd_config
    PasswordAuthentication yes

    # sshd-gen-keys-start
    # service sshd restart

    # netstat -an | grep 22

    # chkconfig --add sshd
    # chkconfig --level 35 sshd on
    # chkconfig --list sshd

## upgrade glibc

    ## http://ftp.gnu.org/gnu/glibc/
    [ghui@StuOS glibc-2.14]$ mkdir build
    [ghui@StuOS glibc-2.14]$ cd build
    [ghui@StuOS build]$ ../configure --prefix=/opt/glibc-2.14
    [ghui@StuOS build]$ make -j4
    [ghui@StuOS build]$ sudo make install
    [ghui@StuOS bin]$ export LD_LIBRARY_PATH=/opt/glibc-2.14/lib:$LD_LIBRARY_PATH
    [ghui@StuOS bin]$ sudo ldconfig -v

## xargs && cp

    # ls *.jpg | xargs -n1 -i cp {} /external-hard-drive/directory

## sudo without password

    If you want to run apt-get without having to supply a sudo password, just edit the sudo config file to allow that. (Replace "zhoujd" in this example with your own login).

    zhoujd ALL=(root) NOPASSWD: /usr/bin/apt-get

    Hint: edit the config file with “sudo visudo”, not “sudo vim /etc/sudoers”. Visudo will check that you haven’t totally screwed up the config file before writing it out.

## multiable file change name

    find public_html/ -name '*.shtml' | perl -pe 's/(.*)\.shtml/ mv $1.shtml $1.php/' | bash

## update initramfs

    # mkinitrd -f -v /boot/initrd-$(uname -r).img $(uname -r)
    # dracut -f
    # dracut -f initramfs-$(uname -r).img $(uname -r)

##  ssh & scp now yes/no ask

    [root@master ~]# vi /etc/ssh/ssh_config
    StrictHostKeyChecking no
    - or -
    [root@master ~]# ssh IP -oUserKnownHostsFile=/dev/null -oStrictHostKeyChecking=no

## disable report problem on ubuntu

    # ls /var/crash/
    # sudo rm -fr /var/crash/*
    # sudo stop apport

    # cat /etc/default/apport
    # set this to 0 to disable apport, or to 1 to enable it
    # you can temporarily override this with
    # sudo service apport start force_start=1
    enabled=0

## tag kernel

    #Tag the patched kernel to provide easy identification.
    $ perl -pi -e 's/.*CONFIG_LOCALVERSION=.*/CONFIG_LOCALVERSION=".ZZ.r1"/' .config

## functions in dynamic library

    $ nm a.so
    $ readelf -s a.so
    $ readelf -a a.so
    $ objdump -x a.so unamed.a

## Change or Rename User Name and UID (user-id)
### Task: View current user and group membership for user named tom

    $ id tom
    $ grep '^tom:' /etc/passwd
    $ grep 'tom' /etc/group
    $ groups tom
    $ ls -ld /home/tom/
    $ ps aux | grep tom
    $ ps -u tom

### Task: Change username from tom to jerry on Linux

    # id tom
    # usermod -l jerry tom
    ## Verify ###
    # id tom
    # id jerry
    # ls -ld /home/tom

    A NOTE ABOUT RUNNING PROCESS
    # usermod -l jerry tom
      usermod: user tom is currently used by process 6886
    # pkill -u tom pid
    # pkill -9 -u tom
    # usermod -l jerry tom

### Task: Change primary groupname from tom to jerry

    # id tom
    # groupmod -n jerry tom
    ## Verify it ###
    # id tom
    # ls -ld /home/tom

### How to change user home directory from /home/tom/ to /home/jerry

    # usermod -d /home/jerry -m jerry
    # usermod -c "jerry" jerry
    # id jerry
    # ls -ld /home/jerry

### How to change user tom UID/GID from 5001 to 10000

    # id tom
    # usermod -u 10000 tom
    # id tom
    # groupmod -g 10000 tom
    # id tom

### Blueman: configured directory for incoming file does not exist

    $ gsettings get org.blueman.transfer shared-path
    $ gsettings set org.blueman.transfer shared-path '/home/your_user_name/Downloads'

### Summary command lines

    # killall -u old
    # id old
    # usermod -l new old
    # groupmod -n new old
    # usermod -d /home/new -m new
    # usermod -c "New Real Name" new
    # id new

## Identify Disk Partition/FileSystem UUID

    # blkid
    # lsblk -o name,mountpoint,size,uuid
    # ls -lh /dev/disk/by-uuid/
    # hwinfo --block | grep by-uuid | awk '{print $3,$7}'
    # udevadm info -q all -n /dev/sdc1 | grep -i by-uuid | head -1
    # tune2fs -l /dev/sdc1 | grep UUID
    # dumpe2fs /dev/sdc1 | grep UUID

## Upgrade GCC (gcc)

    # sudo yum install libmpc-devel mpfr-devel gmp-devel
    # cd ~/Downloads
    # curl ftp://ftp.mirrorservice.org/sites/sourceware.org/pub/gcc/releases/gcc-4.9.2/gcc-4.9.2.tar.bz2 -O
    # curl https://ftp.gnu.org/gnu/gcc/gcc-4.9.2/gcc-4.9.2.tar.bz2 -O

    # tar xvfj gcc-4.9.2.tar.bz2
    # cd gcc-4.9.2
    # ./configure --disable-multilib --enable-languages=c,c++
    # make -j 4
    # make install

## find -print0 and xargs -0

    $ find -name "*.txt" -print0 | xargs -0 sed -i 's/aaa/bbb/g'
    $ find -type f -exec sed -i 's/home/work/g' \{\} \;

## How to Clean a Linux Zombie Process

    $ ps aux | egrep "Z|defunct"
    or
    $ ps -A -ostat,pid,ppid | grep -e '[zZ]'
    Z      108   103
    $ kill -s SIGCHLD 103
    $ kill -9 103

## Show list all users and date the logged in

    ## https://askubuntu.com/questions/1164464/command-to-show-list-all-users-and-date-the-logged-in
    $ users
    $ who -uH
    $ zgrep login /var/log/auth.log*

## Add videos as wallpaper on your Linux desktop

    ## https://opensource.com/article/20/7/linux-wallset
    ## https://github.com/terroo/wallset
    $ wallset -V video.mp4
    $ wallset -L
    $ wallset -I 1

## flatpack install package

    ## https://wiki.archlinux.org/title/Flatpak
    ## ~/.local/share/flatpak/exports/share/applications
    ## /var/lib/flatpak/exports/share/applications
    $ sudo apt install flatpak
    $ sudo apt install gnome-software-plugin-flatpak
    $ flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    Can't load uri https://flathub.org/repo/flathub.flatpakrepo: Unacceptable TLS certificate
    $ flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
    $ flatpak uninstall --unused

## How to use systemctl to list services on systemd Linux

    $ systemctl list-units --type=service
    $ systemctl list-units --type=service --state=running
    $ systemctl list-units --type=service --all
    $ systemctl list-unit-files --state=enabled
    $ systemctl list-unit-files --state=disabled

## Disable sleep on Ubuntu or Red Hat Enterprise Linux

    ## https://www.dell.com/support/kbdoc/zh-cn/000179566/how-to-disable-sleep-and-configure-lid-power-settings-for-ubuntu-or-red-hat-enterprise-linux-7
    ## Disable sleep
    $ sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target
    Created symlink /etc/systemd/system/sleep.target → /dev/null.
    Created symlink /etc/systemd/system/suspend.target → /dev/null.
    Created symlink /etc/systemd/system/hibernate.target → /dev/null.
    Created symlink /etc/systemd/system/hybrid-sleep.target → /dev/null.
    ## Enable sleep
    $ sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target
    ## Status of the standby service
    $ sudo systemctl status sleep.target
    $ sudo systemctl status suspend.target
    $ sudo systemctl status hibernate.target
    $ sudo systemctl status hybrid-sleep.target

## Ubuntu 20.04 System Backup and Restore

    ## Install the Timeshift backup utility
    $ sudo apt install timeshift
    ## Create a first backup
    $ sudo timeshift --create
    ## List all your currently created system backup screenshots
    $ sudo timeshift --list
    ## Restore from the backup snapshot
    $ sudo timeshift --restore --snapshot "2020-02-19_18-32-36"
    ## Delete selected backup snapshot:
    $ sudo timeshift --delete  --snapshot '2014-10-12_16-29-08'

## Snapshot (Deta Changes)

    ## Have to run lvcreate to start the process and then either run lvconvert --merge or lvremove to end it
    ## Create snapshot
    $ lvcreate -s -c 64k -n $NAME /dev/centos/root --size $1

    ## Restore snapshot
    ## Scenario #1: You want to revert changes
    $ sync
    $ vgchange -ay  ## Activate all your volume groups
    $ lvconvert --merge /dev/centos/$NAME -y
    $ reboot

    ## Merge snapshot
    ## Scenario #2: You want to persist changes
    $ lvremove /dev/centos/$NAME -y

    ## Wait for revert
    $ lvs --select 'lv_name=root && lv_attr=~.*O.*'

## Reduce size of your root volume to make some room for a snapshot volume

    ## https://askubuntu.com/questions/988964/how-can-i-use-lvm-snapshots-in-ubuntu
    ## Boot from Live CD
    ## See name of your device
    $ fdisk -l
    ## Decrypt your volume
    $ cryptsetup luksOpen /dev/sda3 crypt1
    ## Find all volume groups
    $ vgscan --mknodes
    ## Activate all your volume groups
    $ vgchange -ay
    ## Reduce size of your root volume by 20 Gb
    $ lvreduce -r -L -20G /dev/ubuntu-vg/root
    ## See that you actually got 20G of free space
    $ vgs
    ## reboot

## Install And Setup TinyProxy

    ## https://github.com/isabelcosta/testing-tiny-proxy
    $ sudo apt install tinyproxy-bin
    $ /etc/init.d/tinyproxy stop
    $ /etc/init.d/tinyproxy start
    $ /etc/init.d/tinyproxy status

    ## Modify configure file for upstream
    $ sudo vim /etc/tinyproxy/tinyproxy.conf
    upstream http proxy-prc.*****.com:913
    $ /etc/init.d/tinyproxy stop
    $ /etc/init.d/tinyproxy start

    ## Test tinyproxy
    $ curl -v --proxy http://127.0.0.1:8888 www.baidu.com
