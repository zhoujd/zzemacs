Void Linux
==========

## URLs

```
## https://docs.voidlinux.org/installation/index.html
## https://docs.voidlinux.org/installation/live-images/guide.html
## https://docs.voidlinux.org/installation/live-images/partitions.html#bios-system-notes
```

## Install Xorg

```
## Xorg
$ sudo xbps-install -Sy xorg

## Build dwm
$ sudo xbps-install -Sy libX11-devel libuuid libXft-devel libXinerama-devel

## Build st
$ sudo xbps-install -Sy gd-devel pkg-config
```
