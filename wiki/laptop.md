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
