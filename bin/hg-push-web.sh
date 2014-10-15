#!/bin/sh

####http support push
##first goto hg project directory
##$hg serve -p 80
##$hg clone http://127.0.0.1 hg-proj
##$hg push

HG_ROOT=`hg root`

# hg repo folder check
if [ ! -s "$HG_ROOT" ] ; then
    echo "`basename $0` should be run under hg repo"
    exit 1
fi

echo "[web]" >>$HG_ROOT/.hg/hgrc
echo "allow_read = *" >>$HG_ROOT/.hg/hgrc
echo "allow_push = *" >>$HG_ROOT/.hg/hgrc
echo "push_ssl   = false" >>$HG_ROOT/.hg/hgrc

echo "HG repo ready to accept command: hg push"
