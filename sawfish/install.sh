#!/bin/sh

echo "install sawfish begin ..."
sudo apt-get install -y sawfish
sudo apt-get install -y gnome-panel xscreensaver fcitx xloadimage gmrun gcolor2 gcalctool

cp .sawfishrc ~

echo "install sawfish end ..."
