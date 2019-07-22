#!/bin/bash

## https://mesonbuild.com/
## https://mesonbuild.com/Tutorial.html
# $ cd /path/to/source/root
# $ meson builddir && cd builddir
# $ ninja
# $ ninja test

## https://mesonbuild.com/Quick-guide.html
# $ cd /path/to/source/root
# $ meson --prefix /usr --buildtype=plain builddir -Dc_args=... -Dcpp_args=... -Dc_link_args=... -Dcpp_link_args=...
# $ ninja -v -C builddir
# $ ninja -C builddir test
# $ DESTDIR=/path/to/staging/root ninja -C builddir install

## Don't use 'sudo apt install meson'
## Remove use 'sudo apt remove meson'
sudo apt-get install python3 python3-pip ninja-build
pip3 install --user meson

echo "Setup meson done"
