#!/bin/sh

## Get source
git clone https://git.gnome.org/browse/meld

## Try to Run
cd meld

## Install to /usr bin 
sudo ln -sf `pwd`/bin/meld /usr/bin
