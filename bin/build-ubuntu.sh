#!/bin/sh

echo "Build emacs for ubuntu begin ..."

echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        sudo apt-get build-dep emacs23
        sudo apt-get install -y build-essential
        sudo apt-get install -y libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev
        sudo apt-get install -y libxaw7-dev libncurses5-dev libgtk2.0-dev librsvg2-dev libdbus-1-dev libgconf2-dev
        sudo apt-get install -y libotf-dev libgpm-dev libm17n-dev libgnutls-dev libselinux1-dev imagemagick
esac

./configure
make
sudo make install

echo "Build emacs for ubuntu end ..."
