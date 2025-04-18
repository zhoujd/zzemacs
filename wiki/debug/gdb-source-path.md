GDB Source Path
===============

## URLs

    ## https://alex.dzyoba.com/blog/gdb-source-path/

## How to point GDB to your sources

    ## Debug info, it has .debug_* section, hence it has debug info.
    $ objdump -h ./python
    $ readelf -S ./python

## How GDB finds source code

    ## To find the sources GDB parses .debug_info section to find all DIEs with tag DW_TAG_compile_unit
    $ objdump -g ./python | vim -
    <11>   DW_AT_name        : (indirect string, offset: 0x10ec): ./Programs/python.c
    <15>   DW_AT_comp_dir    : (indirect string, offset: 0x7a): /home/avd/dev/cpython

    ## parses the .debug_info to find DW_AT_comp_dir with DW_AT_name attributes for the current object file (range of addresses)
    ## opens the file at DW_AT_comp_dir/DW_AT_name
    ## shows the content of the file to you


## How to tell GDB where are the sources

    ## 1. Reconstruct the sources path
    ## You can reconstruct the sources path on the target host,
    ## so GDB will find the source file where it expects.
    ## Stupid but it will work.

    ## 2. Change GDB source path
    (gdb) list
    6	./Programs/python.c: No such file or directory.
    (gdb) directory /usr/src/python
    Source directories searched: /usr/src/python:$cdir:$cwd
    (gdb) list

    ## 3. Set GDB substitution rule
    (gdb) list
    6	./Programs/python.c: No such file or directory.
    (gdb) set substitute-path /home/avd/dev/cpython /usr/src/python
    (gdb) list

    ## 4. Move binary to sources
    $ mv python /home/user/sources/cpython

    ## 5. Compile with -fdebug-prefix-map
    $ make distclean    # start clean
    $ ./configure CFLAGS="-fdebug-prefix-map=$(pwd)=/usr/src/python" --with-pydebug
    $ make -j

    $ objdump -g ./python
    <11>   DW_AT_name        : (indirect string, offset: 0x10ff): ./Programs/python.c
    <15>   DW_AT_comp_dir    : (indirect string, offset: 0x558): /usr/src/python

    ## Conclusion
    ## The easiest ones are directory and set substitute-path commands, though -fdebug-prefix-map is really useful
    ## The '$cdir' (to refer to the compilation directory)
    ## The '$cwd' (to refer to the current working directory)
