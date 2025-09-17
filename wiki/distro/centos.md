CentOS
======

## CentOS time sync

```
$ vim /etc/sysconfig/clock
  ZONE="Asia/Shanghai"  # Zone
  UTC=false             # Close world time sync
  ARC=false

$ ntpdate pool.ntp.org     # sync time
$ /sbin/hwclock --systohc  # sync hardware time and system time
```

## Resize parts

```
$ sudo yum install gparted
```

## Build kernel module

```
## Install deps
$ sudo yum install kernel-devel
$ sudo yum install gcc

## Compile kernel modules
$ sudo ln -s  /usr/src/kernels/3.10.0-1127.19.1.el7.x86_64/ /usr/src/linux
$ sudo make
$ sudo make install
$ sudo insmod e1000e.ko
$ sudo modprobe e1000e
```

## Remove gpgkey

```
[root@apps2 ~]# rpm -q gpg-pubkey --qf '%{NAME}-%{VERSION}-%{RELEASE}\t%{SUMMARY}\n'
gpg-pubkey-f4a80eb5-53a7ff4b    gpg(CentOS-7 Key (CentOS 7 Official Signing Key) <security@centos.org>)
gpg-pubkey-352c64e5-52ae6884    gpg(Fedora EPEL (7) <epel@fedoraproject.org>)
[root@apps2 ~]# rpm -e gpg-pubkey-352c64e5-52ae6884
[root@apps2 ~]# rpm -e --allmatches gpg-pubkey-fe590cb7-533d77ee
[root@apps2 ~]# rpm -q gpg-pubkey --qf '%{NAME}-%{VERSION}-%{RELEASE}\t%{SUMMARY}\n'
gpg-pubkey-f4a80eb5-53a7ff4b    gpg(CentOS-7 Key (CentOS 7 Official Signing Key) <security@centos.org>)
```

## How to add a CentOS repo, having URL of Packages

```
$ cat /etc/yum.repos.d/myrepo.repo
[myrepo]
name=My extras packages for CentOS 7.4.1708
baseurl=baseurl=http://vault.centos.org/centos/7.4.1708/extras/x86_64/
enabled=1
#gpgcheck=1
#gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-mysql

$ sudo yum install --disablerepo=* --enablerepo=myrepo -y docker-1.12.6-55.gitc4618fb.el7.centos
```

## Existing lock /var/run/yum.pid: another copy is running as pid 2287

```
$ sudo rm -f /var/run/yum.pid
$ sudo yum clean all
```

## Fix LC_ALL login error on CentOS

```
## bash: warning: setlocale: LC_ALL: cannot change locale (en_US.utf8)
$ sudo localedef -i en_US -f UTF-8 en_US.UTF-8
```

## Building an RPM Package

```
## 1. Set Up the Environment
$ sudo dnf install -y rpmdevtools rpmlint
$ rpmdev-setuptree

## 2. Prepare the Source Code
$ mkdir hello-0.0.1
$ echo -e '#!/bin/bash\necho "Hello World"' > hello-0.0.1/hello.sh
$ tar -czf hello-0.0.1.tar.gz hello-0.0.1
$ mv hello-0.0.1.tar.gz ~/rpmbuild/SOURCES/

## 3. Create the SPEC File
$ rpmdev-newspec hello
$ cat > ~/rpmbuild/SPECS/hello.spec <<EOF
Name: hello
Version: 0.0.1
Release: 1%{?dist}
Summary: A simple Hello World script

License: GPLv3+
Source0: %{name}-%{version}.tar.gz

BuildArch: noarch
Requires: bash

%description
A demo RPM package for a Hello World script

%prep
%setup -q

%install
mkdir -p %{buildroot}/usr/bin/
cp hello.sh %{buildroot}/usr/bin/

%files
/usr/bin/hello.sh

%changelog
* Mon Oct 02, 2023 Your Name <your.email@example.com> - 0.0.1-1
- Initial package
EOF

## 4. Build the RPM Package
## The resulting .rpm file will be in ~/rpmbuild/RPMS/noarch/
$ rpmbuild -ba ~/rpmbuild/SPECS/hello.spec

## 5. Test and Install
$ sudo rpm -ivh ~/rpmbuild/RPMS/noarch/hello-0.0.1-1.noarch.rpm
$ rpm -qi hello
$ hello.sh

## Best Practices
## Use rpmlint to validate your SPEC file for errors
$ rpmlint ~/rpmbuild/SPECS/hello.spec
```

## How to create a Linux RPM package

```
## https://www.redhat.com/en/blog/create-rpm-package
$ tree ~/rpmbuild/
/home/tux/rpmbuild/
├── BUILD
│   └── hello-0.0.1
│       ├── hello.sh
├── BUILDROOT
├── RPMS
│   └── noarch
│       └── hello-0.0.1-1.el8.noarch.rpm
├── SOURCES
│   └── hello-0.0.1.tar.gz
├── SPECS
│   └── hello.spec
└── SRPMS

## Verify the package has been installed
$ rpm -qi hello
$ rpm -q hello --changelog
$ rpm -ql hello
/usr/bin/hello.sh

## Removing the RPM package
$ sudo dnf remove hello
$ sudo rpm --verbose --erase hello
```
