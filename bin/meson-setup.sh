#!/bin/bash

## https://mesonbuild.com/
## https://mesonbuild.com/Tutorial.html
# $ meson builddir
# $ cd builddir
# $ ninja

sudo apt-get install python3 python3-pip ninja-build
pip3 install --user meson

echo "Setup meson done"
