#!/bin/bash

## rm parameter add -f for force remove
rm_app="rm -ri $1"
sudo_rm_app="sudo $rm_app"

echo "uninstall zzemacs start ..."

echo "remove .emacs ..."
$rm_app ~/.emacs

echo "remove font ..."
$rm_app ~/.fonts/zach
$sudo_rm_app /usr/share/fonts/zach

echo "remove .emacs.d ..."
$rm_app ~/.emacs.d

echo "remove other ..."
$rm_app ~/.terminfo
$rm_app ~/{.gdbinit,.gdbinit.d}
$rm_app ~/{.gitconfig,.gitconfig-url,.gitconfig-work}

echo "uninstall zzemacs end ..."
