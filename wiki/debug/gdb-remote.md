GDB Remote
==========

## URLs

    ## https://davis.lbl.gov/Manuals/GDB/gdb_17.html
    ## https://developer.apple.com/library/archive/documentation/DeveloperTools/gdb/gdb/gdb_18.html

## Debug Remote

    ## On the target machine run:
    gdbserver :5000 yourprogram
    ## On the host machine, run gdb and then load the symbol file:
    (gdb) symbol-file yourprogram
    ## On GDB on the host machine, you then have to connect to connect GDB to the remote gdbserver:
    (gdb) target remote target_ip_address:5000

## Launch gdbserver in Multi-process Mode

    ## On Target, run the gdbserver with –multi and without a program name
    $ gdbserver --multi localhost:2000
    Listening on port 2000

    ## On Host,
    $ gdb

    (gdb) target extended-remote 192.168.1.10:2000
    Remote debugging using 192.168.1.10:2000

    (gdb) set remote exec-file /my_prg
    (gdb) file /my_prg
    Reading symbols from /my_prg...(no debugging symbols found)...done.

    (gdb) b main
    Note: breakpoint 1 also set at pc 0x400550.
    Breakpoint 2 at 0x400550

    (gdb) run
    Starting program: /my_prg
    Breakpoint 1, 0x0000000000400550 in main ()
