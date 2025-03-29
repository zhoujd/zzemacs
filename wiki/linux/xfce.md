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
    Command: cd %f && emacsclient -c -e "(zz/get-shell)"

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
    $ cp /etc/xdg/dunst/dunstrc $HOME/.config/dunst/dunstrc
    $ dunst &
    $ disown
    ## open a new terminal
    $ notify-send "Notification Title" "Notification Messages"

## evolution change keyboard shortcuts

    ## https://askubuntu.com/questions/1125058/how-to-change-evolutions-keyboard-shortcuts
    $ vim ~/.config/evolution/accels

## X cursor

    ## http://web.mit.edu/linux/redhat/docs/Custom-X-Tips.txt
    ## https://pissedoffadmins.com/os/linux/xsetroot-cursor_name-list.html
    $ xsetroot -def
    $ xsetroot -cursor_name top_left_arrow

## picom

    ## https://github.com/yshui/picom
    ## On Ubuntu/Debian
    $ sudo apt install meson
    $ sudo apt install libxext-dev libxcb1-dev libxcb-damage0-dev libxcb-xfixes0-dev libxcb-shape0-dev
    $ sudo apt install libxcb-render-util0-dev libxcb-render0-dev libxcb-randr0-dev libxcb-composite0-dev
    $ sudo apt install libxcb-image0-dev libxcb-present-dev libxcb-xinerama0-dev libxcb-glx0-dev
    $ sudo apt install libpixman-1-dev libdbus-1-dev libconfig-dev libgl1-mesa-dev libpcre2-dev
    $ sudo apt install libpcre3-dev libevdev-dev uthash-dev libev-dev libx11-xcb-dev

    ## On CentOS/Fedora
    $ sudo yum install dbus-devel gcc git libconfig-devel libdrm-devel libev-devel libX11-devel
    $ sudo yum install libX11-xcb libXext-devel libxcb-devel mesa-libGL-devel meson pcre-devel
    $ sudo yum install pixman-devel uthash-devel xcb-util-image-devel xcb-util-renderutil-devel
    $ sudo yum install xorg-x11-proto-devel

    ## Build
    ## Default install prefix is /usr/local
    $ meson configure -Dprefix=<path> build
    $ ninja -C build
    $ ninja -C build install

## Prompt for the password from the user

    ## https://blog.joshgordon.net/quick-and-dirty-rdp-from-linux/
    $ sudo apt install zenity
    $ PASSWORD=$(zenity --password)

## A quit command weaker than windowkill

    ## https://unix.stackexchange.com/questions/159205/a-quit-command-weaker-than-windowkill
    $ xdotool getwindowfocus windowkill
    $ perl -MX11::Protocol -MX11::Protocol::WM -e '$X = X11::Protocol::new(); X11::Protocol::WM::set_wm_protocol($X, ($X->GetInputFocus())[0], "WM_DELETE_WINDOW")'
    $ wmctrl -c :ACTIVE:

## Setup Evolution Email Client on Ubuntu

    $ sudo apt install evolution
    $ sudo apt install evolution-ews

## Set a default user in lightdm

    ## In /etc/lightdm/lightdm.conf Go down the file
    $ sudo vim /etc/lightdm/lightdm.conf
    greeter-hide-users=false

## Set lightdm gtk greeter

    $ cat /etc/lightdm/lightdm-gtk-greeter.conf
    [greeter]
    theme-name = Adwaita-dark
    icon-theme-name = Adwaita
    clock-format = %a %d %b %I:%M %p
    indicators = ~host;~spacer;~session;~clock;~power
