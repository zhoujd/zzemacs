#!/bin/sh

## rm parameter add -f for force remove
rm_app="rm -ri $1"
sudo_rm_app="sudo $rm_app"

echo "uninstall zzemacs start ..."

echo "remove .emacs ..."
$rm_app ~/.emacs

echo "remove fonts ..."
$rm_app ~/.fonts
$sudo_rm_app /usr/share/fonts/zach

echo "remove .emacs.d ..."
$rm_app ~/.emacs.d
$rm_app ~/.zach

echo "remove others ..."
$sudo_rm_app /usr/bin/zz{emacs,run}
$rm_app ~/.terminfo
$rm_app ~/.gdbinit

echo "uninstall zzemacs end ..."
