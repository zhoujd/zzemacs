CentOS
======

## CentOS time sync

    $ vim /etc/sysconfig/clock
      ZONE="Asia/Shanghai"  # Zone
      UTC=false             # Close world time sync
      ARC=false

    $ ntpdate pool.ntp.org     # sync time
    $ /sbin/hwclock --systohc  # sync hardware time and system time

## Resize parts

    $ sudo yum install gparted

## Build kernel module

    ## Install deps
    $ sudo yum install kernel-devel
    $ sudo yum install gcc

    ## Compile kernel modules
    $ sudo ln -s  /usr/src/kernels/3.10.0-1127.19.1.el7.x86_64/ /usr/src/linux
    $ sudo make
    $ sudo make install
    $ sudo insmod e1000e.ko
    $ sudo modprobe e1000e

## Remove gpgkey

    [root@apps2 ~]# rpm -q gpg-pubkey --qf '%{NAME}-%{VERSION}-%{RELEASE}\t%{SUMMARY}\n'
    gpg-pubkey-f4a80eb5-53a7ff4b    gpg(CentOS-7 Key (CentOS 7 Official Signing Key) <security@centos.org>)
    gpg-pubkey-352c64e5-52ae6884    gpg(Fedora EPEL (7) <epel@fedoraproject.org>)
    [root@apps2 ~]# rpm -e gpg-pubkey-352c64e5-52ae6884
    [root@apps2 ~]# rpm -e --allmatches gpg-pubkey-fe590cb7-533d77ee
    [root@apps2 ~]# rpm -q gpg-pubkey --qf '%{NAME}-%{VERSION}-%{RELEASE}\t%{SUMMARY}\n'
    gpg-pubkey-f4a80eb5-53a7ff4b    gpg(CentOS-7 Key (CentOS 7 Official Signing Key) <security@centos.org>)

## How to add a CentOS repo, having URL of Packages

    $ cat /etc/yum.repos.d/myrepo.repo
    [myrepo]
    name=My extras packages for CentOS 7.4.1708
    baseurl=baseurl=http://vault.centos.org/centos/7.4.1708/extras/x86_64/
    enabled=1
    $ sudo yum install --disablerepo=* --enablerepo=myrepo -y docker-1.12.6-55.gitc4618fb.el7.centos
