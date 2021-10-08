Xfce
====

## Install Xfce

    $ sudo apt install -y xubuntu-desktop

    $ sudo apt install -y expect
    $ cat <<EOF | expect
    set timeout -1
    spawn sudo apt install -y xubuntu-desktop
    expect "Default display manager: "
    send "lightdm\n"
    expect eof
    EOF

    $ sudo reboot

## Remove Xfce

    $ sudo apt remove -y xubuntu-desktop xfdesktop4 xfce4-* libxfce4-* lightdm
    $ sudo apt autoremove -y
    $ sudo apt install --reinstall -y gdm3
    $ sudo reboot

## Default wallpaper folder

    $ cd /usr/share/xfce4/backdrops

## Clock format

    Thu Apr 4, 3:24 PM
    %a %b %e, %l:%M %p

## Docky & launchy

    $ sudo apt install docky
    $ gconftool-2 --type Boolean --set /apps/docky-2/Docky/Items/DockyItem/ShowDockyItem False

    $ sudo apt-get install launchy launchy-plugins

    ## Fix the Broken Icons
    ## Fix broken icons for Terminal and Thunar in xfce4 (Docky)
    $ sudo cp /usr/share/applications/* ~/.local/share/applications

## gtk-update-icon-cache: The generated cache was invalid

    ## Icons with spaces:
    $ find -name "* *"
    $ find -name "gnome-control-center *svg" -exec rm {} +
    $ gtk-update-icon-cache --force /usr/share/icons/urutau-icons

## switch workspace via key

    $ sudo apt install wmctrl
    $ wmctrl -s 1,0
    $ wmctrl -s 2,0

## compton compositor

    ## https://wiki.archlinux.org/title/Picom
    $ sudo apt install compton compton-conf

    ## https://wiki.archlinux.org/title/Xcompmgr
    ## xcompmgr -C
    $ sudo apt install xcompmgr

    ## transparency windows
    $ transset -n emacs 0.9
    $ transset -n urxvt 0.9


## window resize

    It's very easy, you can use Alt + right-click + drag.

## Slingscold (Alternative to Launchpad)

    $ sudo add-apt-repository ppa:noobslab/macbuntu
    $ sudo apt-get update
    $ sudo apt-get install slingscold

    ## optional
    $ sudo add-apt-repository --remove ppa:noobslab/macbuntu
    $ sudo apt update

## Xubuntu ISO

    https://xubuntu.org/download
    https://mirrors.tuna.tsinghua.edu.cn/ubuntu-cdimage/xubuntu/releases/18.04/release/

## XFCE4 desktop zooming with the keyboard

    By default, holding the Alt key and scrolling up or down the mouse wheel


## Lightdm etc

    Dezinstalare xscreensaver:
    sudo apt-get purge xscreensaver*

    Instalare LightDM, Light-Locker
    sudo apt-get install lightdm lightdm-gtk-greeter light-locker light-locker-settings

## Disable zoon desktop

    There is an easy way, without losing the moving windows feature.

    Open Settings Editor
    Go to Channel xfwm4
    Disable Property zoom_desktop

## Thunar custom action
    https://docs.xfce.org/xfce/thunar/custom-actions

    Name: Emacs Shell Here
    Description: Open Emacs Shell
    Command: cd %f && emacsclient -c -e "(zz:get-shell)"

    Name: Emacs Shell
    Description: Open Emacs
    Command: emacsclient -c %F

    Name: Open Terminal Here
    Command: exo-open --working-directory %f --launch TerminalEmulator

## Disable evolution-alarm-notify

    $ xfce4-settings-manager
    ## uncheck -> Session and Startup -> Application autostart -> evolution alarm notify
    $ sudo chmod -x  /usr/libexec/evolution-data-server/evolution-alarm-notify

## Ubuntu Evolution fix database disk image is malformed

    ## https://www.richud.com/wiki/Ubuntu_Evolution_fix_database_disk_image_is_malformed
    $ cd ~/.local/share/evolution/mail/local
    $ mv folders.db folders.db-
    $ ~/.cache/evolution/mail/<ews-account-uid>/
    $ find -name "folders.db"


## xdg-open

    ## https://wiki.archlinux.org/title/Xdg-utils
    $ xdg-mime query default video/mp4
    $ xdg-mime default mpv.desktop video/mp4
    $ xdg-mime default mpv.desktop video/webm
    ## For MKV
    $ xdg-mime query filetype whatever.mkv
    video/x-matroska
    $ xdg-mime query default video/x-matroska
    $ xdg-mime default mpv.desktop video/x-matroska

## ulauncher theme

    ## Press Alt+Enter to access an alt menu
    ## https://github.com/yerbestpal/ambiant-mate-ulauncher-theme
    $ mkdir -p ~/.config/ulauncher/user-themes
    $ cd ~/.config/ulauncher/user-themes
    $ git clone https://github.com/yerbestpal/ambiant-mate-ulauncher-theme

## notification with dunst

    ## https://linuxconfig.org/get-better-notifications-in-your-wm-with-dunst
    $ sudo apt install dunst libnotify-bin
    $ mkdir -p $HOME/.config/dunst
    $ cp $XDG_CONFIG_HOME/dunst/dunstrc $HOME/.config/dunst/dunstrc
    $ dunst &
    $ disown
    ## open a new terminal
    $ notify-send "Notification Title" "Notification Messages"
