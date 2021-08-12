HHKB
====

## HHKB Hybrid pairing on Ubuntu 20.04 LTS

    ## https://qiita.com/piyota6/items/194e3e2b8cfcd547a39b
    ## HHKB side
    Fn + q
    Fn + Ctrl + 1 ~ 4

    ## bluetoothctl setting
    $ bluetoothctl
    $ default-agent
    $ scan on
    $ pair [MAC Address]
    $ exit

    ## Login UI bluetooth enable
    $ sudo nano /etc/bluetooth/main.conf
    AutoEnable=true

## HHKB Hybrid pairing on Manjaro

    ## Samilar on Ubuntu
    $ sudo pacman -S bluez-utils

## Turn on bluetooth on login screen

    ## https://unix.stackexchange.com/questions/197212/turn-on-bluetooth-on-login-screen
    $ cat /etc/udev/rules.d/10-local.rules
    # Set bluetooth power up
    ACTION=="add", KERNEL=="hci0", RUN+="/usr/bin/hciconfig hci0 up"

    ## Start bluetooth in rc.local
    $ cat /etc/rc.zach.d/z01bluetooth
    #!/bin/bash

    do_start() {
        echo "$0 start"
        test -x /sbin/rfkill && /sbin/rfkill unblock bluetooth
    }

    do_stop() {
        echo "$0 stop"
    }

    case "$1" in
        start)
            do_start
            ;;
        stop)
            do_stop
            ;;
        *)
            echo "Usage: $0 {start|stop}" >&2
            exit 3
            ;;
    esac
    exit 0

## Bluetooth URLs

    ## https://wiki.archlinux.org/title/Bluetooth

## Bluetooth service failed to set mode

    $ sudo rfkill unblock bluetooth
    $ sudo systemctl stop bluetooth
    $ sudo systemctl status bluetooth
    $ sudo systemctl restart bluetooth
