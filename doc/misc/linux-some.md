Linux something
================

1. rpm package

    <http://linux.vbird.org/linux_basic/0520rpm_and_srpm.php>

2. linux cross reference

    <http://lxr.oss.org.cn/source/>

    <http://lxr.free-electrons.com/>

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
    ls /usr/bin/gcc*
    sudo apt-get install gcc-4.5 gcc-4.5-multilib g++-4.5 g++-4.5-multilib

    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.4 50
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.5 40
    sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.4 50
    sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.5 40

    sudo update-alternatives --config gcc
    sudo update-alternatives --remove gcc /usr/bin/gcc-4.5

5. CentOS rpm
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
    cdrecord -scanbus ==> you well get dev=*,*,*
    cdrecord -v speed=8 dev=0,0,0 -data cd_image.iso

10. CentOS6.5 upgrade gcc 4.7.2
    wget http://people.centos.org/tru/devtools-1.1/devtools-1.1.repo
    sudo cp devtools-1.1.repo /etc/yum.repos.d/devtools-1.1.repo
    sudo yum clean all
    sudo yum install devtoolset-1.1
    scl enable devtoolset-1.1 bash
    gcc –v

