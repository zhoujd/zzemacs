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
