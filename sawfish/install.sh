#!/bin/sh

echo "install sawfish begin ..."

# setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        sudo apt-get install -y sawfish
        sudo apt-get install -y gnome-panel xscreensaver xloadimage gmrun gcolor2 gcalctool
esac

##setup .sawfishrc
ZZSAWFISH_ROOT=`pwd`
rm -f ~/.sawfishrc
echo ";;;this is .sawfishrc for zhoujd.">> ~/.sawfishrc
echo "(defvar zzsawfish-path \"${ZZSAWFISH_ROOT}/\")" >> ~/.sawfishrc
echo "(load-file (concat zzsawfish-path \".sawfishrc\"))" >> ~/.sawfishrc

echo "install sawfish end ..."
