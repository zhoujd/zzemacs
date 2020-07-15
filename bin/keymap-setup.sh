#!/bin/bash

echo "Set capslk to ctrl"

sudo sed -i 's/XKBOPTIONS=.*/XKBOPTIONS="ctrl:nocaps"/' /etc/default/keyboard 
setxkbmap -option "ctrl:nocaps"

echo "Set capslk to ctrl done"
