#!/bin/sh

## https://divansantana.com/teams-on-linux/index.html

setup_app() {
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
}

setup_cap() {
    echo "Setup ~/.mailcap"
    cat > ~/.mailcap <<EOF
x-scheme-handler/msteams; teams %s
EOF
}

setup_app
setup_cap
