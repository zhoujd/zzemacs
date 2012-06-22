#/bin/sh

sudo apt-get install libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev
sudo apt-get install libxaw7-dev libncurses5-dev libgtk2.0-dev librsvg2-dev libdbus-1-dev libgconf2-dev
sudo apt-get install libotf-dev libgpm-dev libm17n-dev libgnutls-dev libselinux1-dev imagemagick

./configure
make
sudo make install

