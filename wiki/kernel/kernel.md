Linux kernel
============

1. Kenerl debug
   https://www.kernel.org/doc/html/v4.16/dev-tools/kgdb.html

2. KGDB debug
   KGDB on target

        # CONFIG_STRICT_KERNEL_RWX is not set
        CONFIG_FRAME_POINTER=y
        CONFIG_KGDB=y
        CONFIG_KGDB_SERIAL_CONSOLE=y

        ## add parameter to initrd in grub.cfg (kgdb over console)
        kgdbwait kgdboc=0,9600      ## ttyS0, 9600/115200

        echo ttyS0 > /sys/module/kgdboc/parameters/kgdboc

        echo g > /proc/sysrq-trigger
        SysRq : DEBUG

    KGDB on host

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

6. Alloc memory

    kmalloc
        kernel kmalloc is returning a virtual address, not a physical one.
        Don't know where exactly nor do we care unless we're doing DMA

    alloc_pages
        If you want to allocate from high memory, use alloc_pages().
        The alloc_pages() function returns a struct page, and not a pointer to a logical address.
        Because high memory might not be mapped, the only way to access it might be via the corresponding struct page structure.
        To obtain an actual pointer, use kmap() to map the high memory into the kernel's logical address space.

    vmalloc
        If you do not need physically contiguous pagesonly virtually contiguoususe vmalloc()

    slab cache
        If you are creating and destroying many large data structures, consider setting up a slab cache.
