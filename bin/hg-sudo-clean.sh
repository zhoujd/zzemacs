#!/bin/sh

#sudo hg status --no-status --unknown -0 | xargs -0 rm
sudo hg status -un | xargs rm
