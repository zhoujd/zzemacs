#!/bin/bash

echo "Set capslk to ctrl"
sudo sed -i 's/XKBOPTIONS=.*/XKBOPTIONS="ctrl:nocaps"/' /etc/default/keyboard 

echo "Set capslk to ctrl done"
