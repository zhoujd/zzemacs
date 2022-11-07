Keyboard
========

## Persistent Keyboard Mapping on Ubuntu using xmodmap

    # https://medium.com/@saplos123456/persistent-keyboard-mapping-on-ubuntu-using-xmodmap-cd01ad828fcd

## Disable resets layouts

    # gnome-settings-daemon that resets layouts when attach a new keyboard
    # or sudo apt install dconf-editor
    dconf write /org/gnome/settings-daemon/plugins/keyboard/active false

## Moving The Ctrl Key on tty console

    https://www.emacswiki.org/emacs/MovingTheCtrlKey

    On Debian and derivatives (Ubuntu, Mint etc.)
    To make Caps Lock another Ctrl key, edit the file /etc/default/keyboard

    $ cat /etc/default/keyboard
    XKBOPTIONS="ctrl:nocaps"		# Some people prefer "ctrl:swapcaps"

    $ sudo dpkg-reconfigure -phigh console-setup
