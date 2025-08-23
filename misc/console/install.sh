#!/bin/bash

echo "Install console font"
sudo cp -fv font/* /usr/share/consolefonts

echo "Set Font"
sudo setfont spleen-8x16.psfu.gz

echo "Install Done"
