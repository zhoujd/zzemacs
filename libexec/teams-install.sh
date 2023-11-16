#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

## https://divansantana.com/teams-on-linux/index.html

install() {
    echo "Setup teams binary to ~/.local/bin"
    ln -sfvT $SCRIPT_ROOT/teams ~/.local/bin/teams

    echo "Setup teams application"
    cat > ~/.local/share/applications/teams.desktop <<EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Teams
Exec=teams
Terminal=false
StartupNotify=true
Categories=AudioVideo;Network;
MimeType=x-scheme-handler/msteams;
X-KDE-Protocols=teams
EOF

    echo "Setup ~/.mailcap"
    cat > ~/.mailcap <<EOF
x-scheme-handler/msteams; teams %s
EOF
}

uninstall() {
    rm -fv ~/.local/bin/teams
    rm -fv ~/.local/share/applications/teams.desktop
    rm -fv ~/.mailcap
}

case $1 in
    install )
        install
        ;;
    uninstall )
        uninstall
        ;;
    * )
        echo "$0 {install|uninstall}"
        ;;
esac
