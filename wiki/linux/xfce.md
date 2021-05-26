Xfce
====

1. Install Xfce

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

2. Remove Xfce

        $ sudo apt remove -y xubuntu-desktop xfdesktop4 xfce4-* libxfce4-* lightdm
        $ sudo apt autoremove -y
        $ sudo apt install --reinstall -y gdm3
        $ sudo reboot

3. Default wallpaper folder

        $ cd /usr/share/xfce4/backdrops

4. Clock format

        Thu Apr 4, 3:24 PM
        %a %b %e, %l:%M %p

5. Docky & launchy

        $ sudo apt install docky
        $ gconftool-2 --type Boolean --set /apps/docky-2/Docky/Items/DockyItem/ShowDockyItem False

        $ sudo apt-get install launchy launchy-plugins

        ## Fix the Broken Icons
        ## Fix broken icons for Terminal and Thunar in xfce4 (Docky)
        $ sudo cp /usr/share/applications/* ~/.local/share/applications


6. gtk-update-icon-cache: The generated cache was invalid

        ## Icons with spaces:
        $ find -name "* *"
        $ find -name "gnome-control-center *svg" -exec rm {} +
        $ gtk-update-icon-cache --force /usr/share/icons/urutau-icons

7. switch workspace via key


        $ sudo apt install wmctrl
        $ wmctrl -s 1,0
        $ wmctrl -s 2,0

8. compton compositor

        $ sudo apt install compton compton-conf

9. window resize

        It's very easy, you can use Alt + right-click + drag.

10. Slingscold (Alternative to Launchpad)

        $ sudo add-apt-repository ppa:noobslab/macbuntu
        $ sudo apt-get update
        $ sudo apt-get install slingscold

        ## optional
        $ sudo add-apt-repository --remove ppa:noobslab/macbuntu
        $ sudo apt update

11. Xubuntu ISO

        https://xubuntu.org/download
        https://mirrors.tuna.tsinghua.edu.cn/ubuntu-cdimage/xubuntu/releases/18.04/release/

12. XFCE4 desktop zooming with the keyboard

        By default, holding the Alt key and scrolling up or down the mouse wheel


13. Lightdm etc

        Dezinstalare xscreensaver:
        sudo apt-get purge xscreensaver*

        Instalare LightDM, Light-Locker
        sudo apt-get install lightdm lightdm-gtk-greeter light-locker light-locker-settings

14. Disable zoon desktop

        There is an easy way, without losing the moving windows feature.

        Open Settings Editor
        Go to Channel xfwm4
        Disable Property zoom_desktop

15. Thunar custom action
    https://docs.xfce.org/xfce/thunar/custom-actions

        Name: Emacs Shell Here
        Description: Open Emacs Shell
        Command: cd %f && emacsclient -c -e "(zz:get-shell)"

        Name: Emacs Shell
        Description: Open Emacs
        Command: emacsclient -c %F

        Name: Open Terminal Here
        Command: exo-open --working-directory %f --launch TerminalEmulator

16. Disable evolution-alarm-notify

        $ xfce4-settings-manager
        ## uncheck -> Session and Startup -> Application autostart -> evolution alarm notify
        $ sudo chmod -x  /usr/libexec/evolution-data-server/evolution-alarm-notify
