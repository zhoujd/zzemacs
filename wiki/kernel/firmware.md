firmware
========

1. How to store firmware file into header file (.h)

```
## Usage: xxd -i fname > abc.h

$ xxd -i somefile.bin
unsigned char somefile_bin[] = {
0x22, 0x22, 0x22, 0x47, 0x75, 0x69, 0x64, 0x65, 0x20, 0x74, 0x68, 0x65,
...
0x69, 0x6f, 0x6e, 0x22, 0x5d, 0x0a, 0x29, 0x0a
};
unsigned int somefile_bin_len = 10568;

$ ld -r -b binary somefile.bin -o somefile.o
$ objdump -x somefile.o
somefile.o: file format ELF32-arm-little

Sections:
Idx Name Size Address Type
0 00000000 0000000000000000
1 .data 00002948 0000000000000000 DATA
2 .symtab 00000050 0000000000000000
3 .strtab 0000004f 0000000000000000
4 .shstrtab 00000021 0000000000000000

SYMBOL TABLE:
00000000 *UND* 00000000
00000000 l d .data 00000000 .data
00000000 .data 00000000 _binary_somefile_bin_start
00002948 .data 00000000 _binary_somefile_bin_end
00002948 *ABS* 00000000 _binary_somefile_bin_size

## To access these symbols in C, we add them as extern to any file that needs them, like so:
extern const char _binary_somefile_bin_start;
extern const char _binary_somefile_bin_end;
extern const int _binary_somefile_bin_size;
```

2. JamesM's kernel development tutorials
   http://www.jamesmolloy.co.uk/tutorial_html/1.-Environment%20setup.html
