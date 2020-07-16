#!/bin/bash

## https://www.emacswiki.org/emacs/EmacsAsDaemon

echo "Setup ~/.config/systemd/user/emacs.service"
mkdir -p ~/.config/systemd/user
tee ~/.config/systemd/user/emacs.service  <<EOF
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
EOF

echo "Start emacs daemon"
systemctl enable --user emacs
systemctl start --user emacs

echo "All Done"
