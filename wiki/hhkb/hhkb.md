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
