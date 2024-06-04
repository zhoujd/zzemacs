elf
===

## URLs

    ## https://linux-audit.com/elf-binaries-on-linux-understanding-and-analysis/

## Tools

    $ scanelf -e /bin/ps
    $ execstack -q /bin/ps

## Command to show full ELF header

    $ hexdump -C -n 64 /bin/ps

## Command to see program headers

    $ dumpelf (pax-utils)
    $ elfls -S /bin/ps
    $ eu-readelf –program-headers /bin/ps

## Commands to see section and headers

    $ dumpelf
    $ elfls -p /bin/ps
    $ eu-readelf –section-headers /bin/ps
    $ readelf -S /bin/ps
    $ objdump -h /bin/ps

## Section groups

    $ readelf -g /bin/ps

## elfutils

    $ /usr/bin/eu-addr2line
    $ /usr/bin/eu-ar – alternative to ar, to create, manipulate archive files
    $ /usr/bin/eu-elfcmp
    $ /usr/bin/eu-elflint – compliance check against gABI and psABI specifications
    $ /usr/bin/eu-findtextrel – find text relocations
    $ /usr/bin/eu-ld – combining object and archive files
    $ /usr/bin/eu-make-debug-archive
    $ /usr/bin/eu-nm – display symbols from object/executable files
    $ /usr/bin/eu-objdump – show information of object files
    $ /usr/bin/eu-ranlib – create index for archives for performance
    $ /usr/bin/eu-readelf – human-readable display of ELF files
    $ /usr/bin/eu-size – display size of each section (text, data, bss, etc)
    $ /usr/bin/eu-stack – show the stack of a running process, or coredump
    $ /usr/bin/eu-strings – display textual strings (similar to strings utility)
    $ /usr/bin/eu-strip – strip ELF file from symbol tables
    $ /usr/bin/eu-unstrip – add symbols and debug information to stripped binary

## elfkickers

    $ /usr/bin/ebfc – compiler for Brainfuck programming language
    $ /usr/bin/elfls – shows program headers and section headers with flags
    $ /usr/bin/elftoc – converts a binary into a C program
    $ /usr/bin/infect – tool to inject a dropper, which creates setuid file in /tmp
    $ /usr/bin/objres – creates an object from ordinary or binary data
    $ /usr/bin/rebind – changes bindings/visibility of symbols in ELF file
    $ /usr/bin/sstrip – strips unneeded components from ELF file

## pax-utils

    $ /usr/bin/dumpelf – dump internal ELF structure
    $ /usr/bin/lddtree – like ldd, with levels to show dependencies
    $ /usr/bin/pspax – list ELF/PaX information about running processes
    $ /usr/bin/scanelf – wide range of information, including PaX details
    $ /usr/bin/scanmacho – shows details for Mach-O binaries (Mac OS X)
    $ /usr/bin/symtree – displays a leveled output for symbols

## prelink

    $ /usr/bin/execstack – display or change if stack is executable
    $ /usr/bin/prelink – remaps/relocates calls in ELF files, to speed up the process
