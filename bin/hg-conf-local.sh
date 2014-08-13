#!/bin/sh

echo -n "please input user.name (Enter for skip): "
read name

echo -n "please input user.email (Enter for skip): "
read email

if [ "$name" != ""  -a "$email" != ""  ] ; then
    hg --config "ui.username=$name<$email>"
fi

echo -n "please input http.proxy (Enter for skip): "
read http_proxy

if [ "$http_proxy" != ""  ] ; then
    hg --config "http_proxy.host=$http_proxy"
fi
