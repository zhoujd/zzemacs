Linux kernel
==============

1. KGDB on target

        CONFIG_KGDB = y
        CONFIG_KGDB_SERIAL_CONSOLE = y

        ## add parameter to initrd in grub.cfg (kgdb over console)
        kgdbwait kgdboc=0,9600      ## ttyS0, 9600/115200

        echo ttyS0 > /sys/module/kgdboc/parameters/kgdboc

        echo g > /proc/sysrq-trigger
        SysRq : DEBUG

2. KGDB on host

        # gdb vmlinux
        (gdb) set remotebaud 9600
        (gdb) target remote /dev/ttyS0

        (gdb) b start_kernel

        (gdb) b panic
        Breakpoint 1 at 0x8033e900: file kernel/panic.c, line 88.
        (gdb) b sys_sync
        Breakpoint 2 at 0x800cd450: file fs/sync.c, line 100.

3. DDD

        ddd --debugger ./arm-eabi-4.4.3/bin/arm-eabi-gdb ./vmlinux

4. gdbtui

        ./arm-eabi-4.4.3/bin/arm-eabi-gdbtui ./vmlinux

5. QEMU
        [root@localhost kvm]# qemu-system-x86_64 -hda vdisk.img -net none -m 1024 -daemonize -cpu host -smp 2 -vnc :1 -s

        $ virsh edit <vm-name>
        <qemu:commandline>
        <qemu:arg value='-S'/>
        <qemu:arg value='-gdb'/>
        <qemu:arg value='tcp::1234'/>
        </qemu:commandline>

