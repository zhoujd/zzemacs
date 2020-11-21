xmodmap
=======

1. Persistent Keyboard Mapping on Ubuntu using xmodmap

    https://medium.com/@saplos123456/persistent-keyboard-mapping-on-ubuntu-using-xmodmap-cd01ad828fcd


2. Disable resets layouts

        # gnome-settings-daemon that resets layouts when attach a new keyboard
        dconf write /org/gnome/settings-daemon/plugins/keyboard/active false

