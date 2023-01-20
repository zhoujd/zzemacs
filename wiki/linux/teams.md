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

## Fix cursor and font on flatseal

    $ flatpak run com.github.tchx84.Flatseal
    ## switch to "Microsoft Teams"
    All system files => ON
    Environment -> XCURSOR_THEME=DMZ-White
    or
    Environment -> XCURSOR_THEME=mintymac-cursors
    Environment -> GTK_THEME=Yaru-Blue-dark

## Remove flatpak package

    $ flatpak uninstall --user --delete-data com.microsoft.Teams

## Download teams

    ## https://packages.microsoft.com/repos/ms-teams/pool/main/t/teams/
    $ wget https://packages.microsoft.com/repos/ms-teams/pool/main/t/teams/teams_1.5.00.10453_amd64.deb
    $ sudo dpkg -i teams_1.5.00.10453_amd64.deb

    ## remove teams
    $ sudo dpkg --remove teams
