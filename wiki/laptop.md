Laptop setting
==============

1. Lock when handle lid switch.
   Change HandleLidSwitch=suspend to lock or ignore, and uncomment it

        $ sudo vim /etc/systemd/logind.conf
          HandleLidSwitch=lock
        or
        $ sed -i '/HandleLidSwitch/d' /etc/systemd/logind.conf >/dev/null 1&>2
        $ echo 'HandleLidSwitch=lock' >> /etc/systemd/logind.conf >/dev/null 1&>2

        $ sudo systemctl restart systemd-logind

2. Disable touchpad using command line.

        ## To turn off the touch pad
        $ synclient TouchpadOff=1
        ## To turn on the touch pad
        $ synclient TouchpadOff=0

3. Clock format

        ## Fri 16 Jul 10 04:22 PM 
        ## %a %d %b %y  %I:%M %p


4. Disable laptop internal key board

        $ fireforx
        https://github.com/zma/usefulscripts/tree/master/script/laptopkb
        
