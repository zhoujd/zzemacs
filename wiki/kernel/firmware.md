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

``` Makefile
## This Makefile will compile every file in SOURCES, then link them together into one ELF binary, 'kernel'. It uses a linker script, 'link.ld' to do this:
SOURCES=boot.o

CFLAGS=
LDFLAGS=-Tlink.ld
ASFLAGS=-felf

all: $(SOURCES) link

clean:
 »  -rm *.o kernel

link:
 »  ld $(LDFLAGS) -o kernel $(SOURCES)

.s.o:
 »  nasm $(ASFLAGS) $<

```

```Link.ld
/* Link.ld -- Linker script for the kernel - ensure everything goes in the */
/*            Correct place.  */
/*            Original file taken from Bran's Kernel Development */
/*            tutorials: http://www.osdever.net/bkerndev/index.php. */

ENTRY(start)
SECTIONS
{
  .text 0x100000 :
  {
    code = .; _code = .; __code = .;
    *(.text)
    . = ALIGN(4096);
  }

  .data :
  {
     data = .; _data = .; __data = .;
     *(.data)
     *(.rodata)
     . = ALIGN(4096);
  }

  .bss :
  {
    bss = .; _bss = .; __bss = .;
    *(.bss)
    . = ALIGN(4096);
  }

  end = .; _end = .; __end = .;
}
```
