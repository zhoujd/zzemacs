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
