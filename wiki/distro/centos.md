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
