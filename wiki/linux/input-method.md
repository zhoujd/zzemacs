Input Method
============

## Rime URLs

    https://rime.im/
    https://rime.im/download/
    https://github.com/rime/ibus-rime
    https://github.com/rime/home/wiki/RimeWithIBus

## Installation

    $ sudo apt install ibus-rime

## Setup Chinese fonts

    # firefox/terminal/chrome/cherrytree

## Configure rime

    # Press Ctrl+` for F4 for menu

## IBus

    $ ibus-setup
    $ ps -ef | grep ibus
    4191       1  0 09:14 ?        00:00:00 /usr/bin/ibus-daemon --daemonize --xim
    4213    4191  0 09:14 ?        00:00:00 /usr/libexec/ibus-memconf
    4216    4191  0 09:14 ?        00:00:00 /usr/libexec/ibus-ui-gtk3
    4218    4191  0 09:14 ?        00:00:00 /usr/libexec/ibus-extension-gtk3
    4222       1  0 09:14 ?        00:00:00 /usr/libexec/ibus-x11 --kill-daemon
    4228    4047  0 09:14 ?        00:00:00 /usr/libexec/ibus-portal
    4340    4191  0 09:14 ?        00:00:00 /usr/libexec/ibus-engine-simple
    4754    4191  0 09:15 ?        00:00:00 /usr/lib/ibus-rime/ibus-engine-rime --ibus
