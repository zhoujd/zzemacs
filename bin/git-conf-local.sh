#!/bin/sh

echo -n "please input user.name (Enter for skip): "
read name
if [ ! "$name" = "" ] ; then
    git config user.name $name
fi

echo -n "please input user.email (Enter for skip): "
read email
if [ ! "$email" = "" ] ; then
    git config user.email $email
fi

echo -n "please input http.proxy  (Enter for skip): "
read http_proxy
if [ ! "$http_proxy" = "" ] ; then
    git config http.proxy $http_proxy
fi
