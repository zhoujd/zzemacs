Kernel Debug
============

## URLs

    ## https://sergioprado.blog/debugging-the-linux-kernel-with-gdb/
    ## https://cs4118.github.io/dev-guides/kernel-debugging.html
    ## https://pwning.systems/posts/setting-up-a-kernel-debugging-environment/

## Preparing Linux for Kernel Debugging

    $ make defconfig
    $ ./scripts/config --set-val CONFIG_DEBUG_INFO  y
    $ ./scripts/config --set-val CONFIG_GDB_SCRIPTS y
    $ make oldconfig

## Build Options for Debug

    $ cat .config | grep CONFIG_
    CONFIG_DEBUG_INFO=y
    CONFIG_MAGIC_SYSRQ=y
    CONFIG_MAGIC_SYSRQ_DEFAULT_ENABLE=0x1
    CONFIG_MAGIC_SYSRQ_SERIAL=y
    CONFIG_KGDB=y
    CONFIG_KGDB_KDB=y
    CONFIG_KDB_DEFAULT_ENABLE=0x0
    CONFIG_KDB_KEYBOARD=y
    CONFIG_KDB_CONTINUE_CATASTROPHIC=0
    # CONFIG_SERIAL_KGDB_NMI is not set
    CONFIG_HAVE_ARCH_KGDB=y
    CONFIG_KGDB_SERIAL_CONSOLE=y
    CONFIG_KGDB_TESTS=y
    # CONFIG_KGDB_TESTS_ON_BOOT is not set
    CONFIG_KGDB_LOW_LEVEL_TRAP=y


## Debug crash

    ## https://walac.github.io/kernel-crashes/
    ## https://ubuntu.com/server/docs/kernel-crash-dump
    $ sudo apt install linux-crashdump
    $ cat /proc/cmdline
    BOOT_IMAGE=/vmlinuz-3.2.0-17-server root=/dev/mapper/PreciseS-root ro
     crashkernel=384M-2G:64M,2G-:128M
    $ dmesg | grep -i crash
    $ kdump-config show

    ## Testing the crash dump mechanism
    $ cat /proc/sys/kernel/sysrq
    $ sudo sysctl -w kernel.sysrq=1

## Disable ASLR

    ## On Kernl Build
    ## CONFIG_RANDOMIZE_BASE to n, then build kernel

    ## On Kernel Boot
    ## Add 'nokaslr' to GRUB_CMDLINE_LINUX_DEFAULT
    $sudo vi /etc/default/grub
    GRUB_CMDLINE_LINUX_DEFAULT="quiet splash nokaslr"
    $ sudo update-grub
