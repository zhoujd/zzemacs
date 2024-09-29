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
    CONFIG_DEBUG_INFO=y                     # 编译debug info
    CONFIG_MAGIC_SYSRQ=y                    # 功能编译开关
    CONFIG_MAGIC_SYSRQ_DEFAULT_ENABLE=0x1   # 默认开启项 默认存在 /proc/sysrq-trigger
    CONFIG_MAGIC_SYSRQ_SERIAL=y             # 串行sysrq （许多嵌入式主板都具有断开连接的 TTL 电平串行，这可能会产生一些无效的信号。导致检测到虚假sysrq ）
    CONFIG_KGDB=y                           # 需要开启 gdb 远程调试内核
    CONFIG_KGDB_KDB=y                       # KDB调试前端
    CONFIG_KDB_DEFAULT_ENABLE=0x0           # 默认不启动KDB 通过模块拔插方式开启
    CONFIG_KDB_KEYBOARD=y                   # 需要开启 可以使用键盘
    CONFIG_KDB_CONTINUE_CATASTROPHIC=0      # KDB调试内核引发的Oops等容错，=0 总是继续 =1 尝试继续 =2 强制重新启动
    # CONFIG_SERIAL_KGDB_NMI is not set     # 此特殊驱动程序允许您临时使用 NMI 调试器端口作为普通控制台（假设该端口已连接到 KGDB）
    CONFIG_HAVE_ARCH_KGDB=y                 # 依赖项
    CONFIG_KGDB_SERIAL_CONSOLE=y            # KGDB 串口调试终端配置，与 kgdb 共享串行控制台。初始化必须使用 Sysrq -g 进入
    CONFIG_KGDB_TESTS=y                     # 这是一个 kgdb I/O 模块，专门用于测试kgdb 的内部功能。回归测试使用
    # CONFIG_KGDB_TESTS_ON_BOOT is not set  # 启动时进行模块内部测试
    CONFIG_KGDB_LOW_LEVEL_TRAP=y            # 这将为断点异常处理程序添加对 kgdb 的额外回调，这将允许 kgdb 单步执行。


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
