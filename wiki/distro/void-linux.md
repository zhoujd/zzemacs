Void Linux
==========

## URLs

```
## https://docs.voidlinux.org/installation/index.html
## https://docs.voidlinux.org/installation/live-images/guide.html
## https://docs.voidlinux.org/installation/live-images/partitions.html#bios-system-notes
## https://ulyssesgoods.ru/posts/install-void-on-desktop/
## https://franza.dev/notes/void/
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

## Display Manager and Desktop

```
$ xbps-install xorg lightdm lightdm-gtk-greeter elogind
$ xbps-install xfce4
$ vi /etc/passwd
root:x:0:0:root:/root:/bin/bash
```

## Sound

```
## Install ALSA
$ xbps-install alsa-firmware alsa-utils
$ ln -s /etc/sv/alsa /etc/runit/runsvdir/default

## Install PipeWire and GUI
$ xbps-install pipewire wireplumber pavucontrol
$ mkdir -p /etc/pipewire/pipewire.conf.d
$ ln -s /usr/share/examples/wireplumber/10-wireplumber.conf /etc/pipewire/pipewire.conf.d/
$ ln -s /usr/share/examples/pipewire/20-pipewire-pulse.conf /etc/pipewire/pipewire.conf.d/
```

## Troubleshoot

```
## Deleted /var/service/
$ sudo ln -s /var/run/runit/runsvdir/current /var/service
$ xbps-install -f runit
```
