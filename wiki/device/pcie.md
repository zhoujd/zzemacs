PCIE
====

## Interpreting the output of lspci

```
## https://dassencio.org/75
## As the PCI specification allows for up to 256 buses
## Each bus can support up to 32 devices, and each PCI device can have up to eight functions.
## In technical terms, a device's location is denoted by a 16-bit domain number,
## an 8-bit bus number, a 5-bit device number, and a 3-bit function number.
## The last three numbers are frequently referred to as the device's BDF or B/D/F (standing for bus/device/function).
## The bus number 00, device number 1f, and function number 2
$ lspci -nn
00:1f.2 SATA controller: Intel Corporation Lynx Point-LP SATA Controller 1 [AHCI mode] (rev 04)

$ lspci -D
$ lspci -tv
$ lspci -v
$ lspci -v -s 00:1f.2
$ lspci -vmm
$ lspci -xnn

$ man lspci
$ sudo dmidecode -s
```
