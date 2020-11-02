CentOS
======

1. CentOS time sync

        $ vim /etc/sysconfig/clock
          ZONE="Asia/Shanghai"  # Zone
          UTC=false             # Close world time sync
          ARC=false
        
        $ ntpdate pool.ntp.org    # sync time
        $ /sbin/hwclock --systohc  # sync hardware time and system time

2. Resize parts

        $ sudo yum install gparted

3. Build kernel module

        ## Install deps
        $ sudo yum install kernel-devel
        $ sudo yum install gcc

        ## Compile kernel modules
        $ sudo ln -s  /usr/src/kernels/3.10.0-1127.19.1.el7.x86_64/ /usr/src/linux
        $ sudo make
        $ sudo make install
        $ sudo insmod e1000e.ko
        $ sudo modprobe e1000e
