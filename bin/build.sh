#/bin/sh

sudo apt-get install libxaw7-dev libxpm-dev libpng12-dev libjpeg-dev libtiff4-dev libgif-dev
sudo apt-get install libncurses5-dev libgtk2.0-dev librsvg2-dev libdbus-1-dev libgconf2-dev

./configure
make
sudo make install

