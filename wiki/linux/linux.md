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
    # sudo systemctl stop apport

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

## Upgrade GCC (gcc) via build from source

    # sudo yum install libmpc-devel mpfr-devel gmp-devel
    # cd ~/Downloads
    # ver=7.5.0
    # curl ftp://ftp.mirrorservice.org/sites/sourceware.org/pub/gcc/releases/gcc-${ver}/gcc-${ver}.tar.gz -O
    # curl https://ftp.gnu.org/gnu/gcc/gcc-${ver}/gcc-${ver}.tar.gz -O

    # tar xvf gcc-${ver}.tar.gz
    # cd gcc-${ver}
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
    $ sudo systemctl status sleep.target suspend.target hibernate.target hybrid-sleep.target

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

## Laptop Battery status

    $ acpi -b | awk '{ print $3, $4 }' | tr -d ','


## How do I find out what hard disks are in the system

    $ sudo fdisk -l 2>/dev/null | grep "Disk \/" | grep -v "\/dev\/md" | awk '{print $2}' | sed -e 's/://g'
    /dev/nvme0n1
    /dev/nvme1n1

## How to mount a new drive on startup

    ## https://askubuntu.com/questions/154180/how-to-mount-a-new-drive-on-startup
    $ sudo fdisk /dev/sdb
    Press O and press Enter (creates a new table)
    Press N and press Enter (creates a new partition)
    Press P and press Enter (makes a primary partition)
    Then press 1 and press Enter (creates it as the 1st partition)
    Finally, press W (this will write any changes to disk)

    $ sudo mkfs.ext4 /dev/sdb1
    $ sudo vim /etc/fstab
    #device        mountpoint             fstype    options  dump   fsck
    /dev/sdb1      /home/yourname/mydata    ext4    defaults    0    1

    $ sudo reboot

## Cannot remove a printer, keeps returning,

    ## edit /etc/cups/cups-browsed.conf
    ## Make the the following change:
    BrowseRemoteProtocols none

    $ sudo systemctl restart cups

## Limit folder size

    $ touch 2gbarea
    $ truncate -s 2G 2gbarea
    $ mke2fs -t ext4 -F 2gbarea
    mke2fs 1.43.3 (04-Sep-2016)
    Discarding device blocks: done
    Creating filesystem with 524288 4k blocks and 131072 inodes
    Filesystem UUID: bf1b2ee8-a7df-4a57-9d05-a8b60323e2bf
    Superblock backups stored on blocks:
        32768, 98304, 163840, 229376, 294912

    Allocating group tables: done
    Writing inode tables: done
    Creating journal (16384 blocks): done
    Writing superblocks and filesystem accounting information: done

    $ sudo mount 2gbarea up
    $ df -h up
    Filesystem      Size  Used Avail Use% Mounted on
    /dev/loop0      2.0G  6.0M  1.8G   1% /home/muru/up

## Change UID and GID

    ## To assign a new UID to user called foo, enter:
    $ usermod -u 2005 foo

    ## To assign a new GID to group called foo, enter:
    $ groupmod -g 3000 foo

## Cannot connect lftp to IIS FTP with SSL

    ## using a bash script
    $ cat lftp-script.sh
    #!/bin/bash
    USER='username'
    PASS='password'
    HOST='ftp.mydomain.com'
    LOCAL_BACKUP_DIR='/backups'
    REMOTE_DIR='/backupfiles'

    lftp -u $USER,$PASS $HOST <<EOF
    set ftp:ssl-protect-data true
    set ftp:ssl-force true
    set ssl:verify-certificate no
    mirror -R -e "$LOCAL_BACKUP_DIR" "$REMOTE_DIR"
    quit
    EOF

    ## Using a single command line
    $ lftp -u $USER,$PASS -e "set ftp:ssl-protect-data true set ftp:ssl-force true set ssl:verify-certificate no" $HOST -p $PORT

## Listing the contents of the local directory in ftp

    ## ! means locally not the remote
    ## lcd is working but !cd will not work and lpwd is not working but !pwd is working
    ftp > !dir
    ftp > !ls

## Modify CPU frequence

    $ echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
    $ cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
    $ lscpu

## Faketime for time test

    ## https://github.com/wolfcw/libfaketime/
    $ sudo apt install -y faketime
    $ faketime '2008-12-24 08:15:42' python test.py

## Find with cp combine

    ## https://stackoverflow.com/questions/5241625/find-and-copy-files
    $ find /mydir -type f -name 'log*.txt' | tail -n 10 | xargs -I % cp % /tmp/mydir/
    $ find -mtime -1 -type f -exec cp '{}' inner/ \;
    $ find -mtime -1 -type f | xargs cp -t inner/

## Oneshot systemd type

    ## service will exit after app exit
    [Service]
    Type=oneshot

## Run a Script at a Certain Time

    ## https://www.baeldung.com/linux/schedule-script-execution
    # Cron (Period)
    $ sudo apt install cron -y
    $ crontab -l
    $ export EDITOR=vi
    $ crontab -e
    # [minute] [hours] [day of month] [month] [day of the week] command-to-execute
    0 5 * * 1 tar -zcf /var/backups/home.tgz /home/

    ## at (only once)
    $ sudo apt install at -y
    $ at 09:00 -f /home/baeldung/one-time-env-setup.sh

    ## batch (only once, based on CPU load)
    ## When the system’s average CPU load is less than 1.5, then the system will execute the scheduled commands
    $ batch

## Analyze binary files

    ## https://opensource.com/article/20/4/linux-binary-analysis
    $ file /bin/ls
    $ ldd /bin/ls
    $ ltrace ls
    $ hexdump -C /bin/ls | head
    $ readelf -h /bin/ls
    $ objdump -d /bin/ls | head
    $ strace -f /bin/ls
    $ nm hello | tail
    $ gdb -q ./hello

## How to get only the first ten bytes of a binary file

    ## https://stackoverflow.com/questions/4411014/how-to-get-only-the-first-ten-bytes-of-a-binary-file
    ## To get the first 10 bytes
    $ head -c 10
    ## To get all but the first 10 bytes
    $ tail -c+11

    ## Using head or dd but in a single pass
    $ { head -c 10 >head_part; cat >tail_part;} <file
    $ { dd count=1 bs=10 of=head_part; cat;} <file >tail_part

## LD_PRELOAD to replace dynamic library

    $ export LD_PRELOAD="./myhack.so"
    $ unset LD_PREDLOAD
    $ ldd

## SSH keys and autologin

    $ cd ~/.ssh
    $ chmod 600 id_rsa id_rsa.pub
    $ chmod 644 authorized_keys

## How to disable fsck on reboot in linux

    ## https://unixadminguide.blogspot.com/2013/12/how-to-disable-fsck-on-reboot-in-linux.html
    ## Filesystem tunable
    $ sudo tune2fs -c 0 /dev/sda2
    $ tune2fs /dev/sda2 | egrep -i 'mount count|check'

    ## To disable fsck check on /dev/sda2
    $ tune2fs -c 0 -i 0 /dev/sda2
    $ tune2fs /dev/sda2 | egrep -i 'mount count|check'

    ## Grub boot parameter
    ## Add the following at the end of your grub boot linux line.
    ## fastboot

    ## Placing command files on your root device
    To disable the filesystem check on boot.
    $ sudo touch /fastboot
    ## To enable a filesystem check on boot.
    $ sudo touch /forcefsck

    ## update /etc/fstab
    $ grep nofsck /etc/fstab
    /dev/sda2        /mnt/nofsck        ext4        defaults        0  0

    ## Active reboot without FSCK
    $ sudo shutdown -rf
