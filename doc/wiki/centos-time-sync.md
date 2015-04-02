CentOS time sync
==================================

1. shell> vim /etc/sysconfig/clock

         ZONE="Asia/Shanghai"  # Zone
         UTC=false             # Close world time sync
         ARC=false

   :wq!

2. shell> ntpdate pool.ntp.org    # sync time
3. shell>/sbin/hwclock --systohc  # sync hardware time and system time
