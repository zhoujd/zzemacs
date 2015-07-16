#!/bin/sh

## rm parameter
rm_app="rm -ri $1"
sudo_rm_app="sudo $rm_app"

echo "uninstall zzemacs start ..."

echo "remove .emacs ..."
$rm_app ~/.emacs

echo "remove fonts ..."
$rm_app ~/.fonts

echo "remove .emacs.d ..."
$rm_app ~/.emacs.d

echo "remove /usr/bin/zzemacs ..."
$sudo_rm_app /usr/bin/zzemacs

echo "uninstall zzemacs end ..."
