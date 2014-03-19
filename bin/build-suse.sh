#!/bin/sh

echo "Build emacs for suse begin ..."

echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        sudo zypper install libjpeg-devel libpng-devel giflib-devel libtiff-devel
esac

./configure
make
sudo make install

echo "Build emacs for suse end ..."
