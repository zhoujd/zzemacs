README
======

## Linux Kernel Userptr Example

```
## 1 build ko
$ make
$ sudo insmod userptr_example.ko

## 2 build test
$ gcc -o test test.c
$ sudo ./test
$ sudo dmesg | tail 
```
