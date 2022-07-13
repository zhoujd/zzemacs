Laptop setting
==============

## Lock when handle lid switch.
   Change HandleLidSwitch=suspend to lock or ignore, and uncomment it

    $ sudo vim /etc/systemd/logind.conf
      HandleLidSwitch=lock
    or
    $ sed -i '/HandleLidSwitch/d' /etc/systemd/logind.conf >/dev/null 1&>2
    $ echo 'HandleLidSwitch=lock' >> /etc/systemd/logind.conf >/dev/null 1&>2

    $ sudo systemctl restart systemd-logind

## Disable touchpad using command line.

    ## To turn off the touch pad
    $ synclient TouchpadOff=1
    ## To turn on the touch pad
    $ synclient TouchpadOff=0

    ## Disable tap to click
    $ synclient TapButton1=0
    ## Enable tap to click
    $ synclient TapButton1=1

## Clock format

    ## Fri 16 Jul 10 04:22 PM
    ## %a %d %b %y  %I:%M %p

## Disable laptop internal key board

    $ firefox https://github.com/zma/usefulscripts/tree/master/script/laptopkb
