#!/bin/sh

hg status -u0 | xargs -0 sudo rm -f
