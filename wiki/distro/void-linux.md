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

## URLs

```
## https://docs.voidlinux.org/installation/index.html
## https://docs.voidlinux.org/installation/live-images/guide.html
## https://docs.voidlinux.org/installation/live-images/partitions.html#bios-system-notes
```

## Install Xorg

```
## WM tool
$ sudo xbps-install -Sy bash-completion
$ sudo xbps-install -Sy wmctrl xdotool psmisc

## Xorg
$ sudo xbps-install -Sy xorg

## Build dwm
$ sudo xbps-install -Sy libX11-devel libuuid libXft-devel libXinerama-devel

## Build st
$ sudo xbps-install -Sy gd-devel pkg-config

## Build dwmstatus
$ sudo xbps-install -Sy alsa-lib-devel

## Build cwm
$ sudo xbps-install libXrandr-devel byacc

## Build xbanish
$ sudo xbps-install libXfixes-devel libXi-devel

## Build xcompmgr
$ sudo xbps-install libXcomposite-devel libXdamage-devel
```

## VirtualBox Addtions

```
$ sudo xbps-install linux-headers
```
