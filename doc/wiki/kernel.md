Linux kernel
==============

1. KGDB on target

        CONFIG_KGDB = y
        CONFIG_KGDB_SERIAL_CONSOLE = y 

        ## add parameter to initrd in grub.cfg (kgdb over console)
        kgdbwait kgdboc=0,9600      ## ttyS0, 9600
        
        echo ttyS0 > /sys/module/kgdboc/parameters/kgdboc

        echo g > /proc/sysrq-trigger
        SysRq : DEBUG

2. KGDB on host

        # gdb vmlinux
        (gdb) set remotebaud 9600
        (gdb) target remote /dev/ttyS0
