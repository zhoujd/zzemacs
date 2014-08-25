#!/bin/sh

####http support push
##first goto hg project directory
##$hg serve -p 80
##$hg clone http://127.0.0.1 hg-proj
##$hg push

echo "[web]" >>.hg/hgrc
echo "allow_read = *" >>.hg/hgrc
echo "allow_push = *" >>.hg/hgrc
echo "push_ssl   = false" >>.hg/hgrc

