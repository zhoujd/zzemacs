teams
=====

## Install teams with flatpak

    ## https://clearlinux.org/software/flathub/microsoft-teams
    $ flatpak install flathub com.microsoft.Teams
    $ flatpak run com.microsoft.Teams
    $ flatpak --help
    $ sudo tee /usr/local/bin/teams <<EOF
    #!/bin/bash
    exec flatpak run com.microsoft.Teams
    EOF
    $ sudo chmod +x /usr/local/bin/teams

## Fix the cursor

    $ sudo flatpak override --env XCURSOR_THEME=DMZ-White
    $ mkdir -p ~/.icons/default/cursors/
    $ cp -R /usr/share/icons/DMZ-White/cursors/. ~/.icons/default/cursors/
