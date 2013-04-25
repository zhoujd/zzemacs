#!/bin/sh

echo install linux-tools begin...
sudo apt-get install -y cairo-dock cairo-dock-plug-ins

##develop package
sudo apt-get install -y gnome-core-devel
sudo apt-get install -y libx11-dev
sudo apt-get install -y xorg-dev

echo install linux-tools end...
