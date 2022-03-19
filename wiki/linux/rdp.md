RDP
===

## xfreerdp

    $ sudo apt install freerdp2-x11 freerdp2-shadow-x11
    $ xfreerdp
    $ cat ~/.jiandon-mobl
    /t:jiandon-mobl
    /smart-sizing
    /w:1920
    /h:1080
    /sound
    /microphone
    /clipboard
    /disp
    $ xfreerdp ~/.jiandon-mobl.rdp -grab-keyboard \
      /u:<domain>\<user> \
      /p:<passwd> \
      /v:<host>
