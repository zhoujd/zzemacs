Linux something
================

1. rpm package

    <http://linux.vbird.org/linux_basic/0520rpm_and_srpm.php>

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
    # dd if=/dev/sda1 of=sda1.img bs=4M
    # dd if=sda1.img.bak of=/dev/sda1

    # dd if=/dev/sda bs=1M | gzip -c > sda1.img.gz
    # gzip -cd sda1.img.gz | dd of=/dev/sda1

    # dd if=/dev/sda1 | bzip2 > sda1.img.bz2
    # bzip2 -dc sda1.img.bz2 | dd of=/dev/sda1

    # e2fsck -f /dev/sda1
    # resize2fs /dev/sda1
    # e2fsck -f /dev/sda1

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
