Linux something
================

1. rpm package

    <http://linux.vbird.org/linux_basic/0520rpm_and_srpm.php>

    $ ls *rpm | egrep -v "samples" | xargs sudo yum -y localinstall
    $ sudo yum -y groupinstall "development tools"
    $ sudo yum -y localinstall *.rpm

    $ yum list installed | grep kmd

2. linux cross reference

    <http://lxr.oss.org.cn/source/>

    <http://lxr.free-electrons.com/>

    <http://lxr.linux.no/>

3. FreeBSD & Linux cross refernce

    <http://fxr.watson.org/>

    <http://svnweb.freebsd.org/>

3. android cross reference

    <http://androidxref.com/>

    <http://code.metager.de/source/xref/android/>

4. QEMU

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

4. Multi GCC

    gcc -v
    ls -l /usr/bin/gcc* /usr/bin/g++*
    sudo apt-get install gcc-4.7 gcc-4.7-multilib g++-4.7 g++-4.7-multilib

    sudo update-alternatives --remove gcc /usr/bin/gcc-4.6
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.7 60 --slave /usr/bin/g++ g++ /usr/bin/g++-4.7
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 40 --slave /usr/bin/g++ g++ /usr/bin/g++-4.6

    sudo update-alternatives --config gcc

5. CentOS rpm
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

6. Centos LIB dir when compile
    ./configure --prefix=/usr  --libdir=/usr/lib64

7. Enter text console
    SUSE/CentOS: vim /etc/inittab  5=>3
    Ubuntu: vim /etc/default/grub
            GRUB_CMDLINE_LINUX_DEFAULT="quiet splash text"
            sudo update-grub

8. Q/A web for Linux/FreeBSD/Unix like
    http://stackexchange.com/

9. CD/DVD image burning
    mkisofs -r -o sample.iso my_private

    cdrecord -scanbus ==> you well get dev=*,*,*
    cdrecord -v speed=8 dev=0,0,0 -data cd_image.iso
    cdrecord -v -eject dev=0,0,0 -data cd_image.iso

10. CentOS6.5 upgrade gcc 4.7.2
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

11. Ubuntu proxy
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

12. Ubuntu ISO image
    http://releases.ubuntu.com/12.04.5/
    http://cdimage.ubuntu.com/releases/
    http://mirrors.163.com/ubuntu-releases/

13. dd backup disk
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

14. SUSE ssh access
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

15. upgrade glibc
    ## http://ftp.gnu.org/gnu/glibc/
    [ghui@StuOS glibc-2.14]$ mkdir build
    [ghui@StuOS glibc-2.14]$ cd build
    [ghui@StuOS build]$ ../configure --prefix=/opt/glibc-2.14
    [ghui@StuOS build]$ make -j4
    [ghui@StuOS build]$ sudo make install
    [ghui@StuOS bin]$ export LD_LIBRARY_PATH=/opt/glibc-2.14/lib:$LD_LIBRARY_PATH
    [ghui@StuOS bin]$ sudo ldconfig -v

16. xargs && cp
    # ls *.jpg | xargs -n1 -i cp {} /external-hard-drive/directory

17. sudo without password
    If you want to run apt-get without having to supply a sudo password, just edit the sudo config file to allow that. (Replace "zhoujd" in this example with your own login).

    zhoujd ALL=(root) NOPASSWD: /usr/bin/apt-get

    Hint: edit the config file with “sudo visudo”, not “sudo vim /etc/sudoers”. Visudo will check that you haven’t totally screwed up the config file before writing it out.

18. multiable file change name
    find public_html/ -name '*.shtml' | perl -pe 's/(.*)\.shtml/ mv $1.shtml $1.php/' | bash

19. update initramfs
    # mkinitrd -f -v /boot/initrd-$(uname -r).img $(uname -r)
    # dracut -f
    # dracut -f initramfs-$(uname -r).img $(uname -r)

20. ssh&scp now yes/no ask
    [root@master ~]# vi /etc/ssh/ssh_config
    StrictHostKeyChecking no
    - or -
    [root@master ~]# ssh IP -oUserKnownHostsFile=/dev/null -oStrictHostKeyChecking=no

21. disable report problem on ubuntu
    # ls /var/crash/
    # sudo rm -fr /var/crash/*
    # sudo stop apport

    # cat /etc/default/apport
    # set this to 0 to disable apport, or to 1 to enable it
    # you can temporarily override this with
    # sudo service apport start force_start=1
    enabled=0

21. tag kernel
    #Tag the patched kernel to provide easy identification.
    $ perl -pi -e 's/.*CONFIG_LOCALVERSION=.*/CONFIG_LOCALVERSION=".ZZ.r1"/' .config

22. functions in dynamic library
    $ nm a.so
    $ readelf -s a.so
    $ readelf -a a.so
    $ objdump -x a.so unamed.a

23. Change or Rename User Name and UID (user-id)
    Task: View current user and group membership for user named tom
        $ id tom
        $ grep '^tom:' /etc/passwd
        $ grep 'tom' /etc/group
        $ groups tom
        $ ls -ld /home/tom/
        $ ps aux | grep tom
        $ ps -u tom
        
    Task: Change username from tom to jerry on Linux
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
        
    Task: Change primary groupname from tom to jerry
        # id tom
        # groupmod -n jerry tom
        ## Verify it ###
        # id tom
        # ls -ld /home/tom
        
    How to change user home directory from /home/tom/ to /home/jerry
        # usermod -d /home/jerry -m jerry
        # usermod -c "jerry" jerry
        # id jerry
        # ls -ld /home/jerry
        
    How to change user tom UID/GID from 5001 to 10000
        # id tom
        # usermod -u 10000 tom
        # id tom
        # groupmod -g 10000 tom
        # id tom

    Blueman: configured directory for incoming file does not exist
        $ gsettings get org.blueman.transfer shared-path
        $ gsettings set org.blueman.transfer shared-path '/home/your_user_name/Downloads'

    Summary command lines
        # killall -u old
        # id old
        # usermod -l new old
        # groupmod -n new old
        # usermod -d /home/new -m new
        # usermod -c "New Real Name" new
        # id new


24. 7 Methods To Identify Disk Partition/FileSystem UUID
     
    # blkid
    # lsblk -o name,mountpoint,size,uuid
    # ls -lh /dev/disk/by-uuid/
    # hwinfo --block | grep by-uuid | awk '{print $3,$7}'
    # udevadm info -q all -n /dev/sdc1 | grep -i by-uuid | head -1
    # tune2fs -l /dev/sdc1 | grep UUID
    # dumpe2fs /dev/sdc1 | grep UUID

25. Upgrade GCC (gcc)


    # sudo yum install libmpc-devel mpfr-devel gmp-devel
    # cd ~/Downloads
    # curl ftp://ftp.mirrorservice.org/sites/sourceware.org/pub/gcc/releases/gcc-4.9.2/gcc-4.9.2.tar.bz2 -O
    # curl https://ftp.gnu.org/gnu/gcc/gcc-4.9.2/gcc-4.9.2.tar.bz2 -O

    # tar xvfj gcc-4.9.2.tar.bz2
    # cd gcc-4.9.2
    # ./configure --disable-multilib --enable-languages=c,c++
    # make -j 4
    # make install
