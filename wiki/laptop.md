Laptop setting
==============

1. Lock when handle lid switch.
   Change HandleLidSwitch=suspend to lock, and uncomment it.

        $ sudo vim /etc/systemd/logind.conf
          HandleLidSwitch=lock
