#!/bin/sh

echo -n "Please input user.name (Enter for skip): "
read name
if [ ! "$name" = "" ] ; then
    git config user.name $name
fi

echo -n "Please input user.email (Enter for skip): "
read email
if [ ! "$email" = "" ] ; then
    git config user.email $email
fi

echo -n "Please input http.proxy (Enter for skip): "
read http_proxy
if [ ! "$http_proxy" = "" ] ; then
    git config http.proxy $http_proxy
fi

echo "Hint core.autocrlf (auto: for window, input: for linux)"
echo -n "Please input core.autocrlf (Enter for skip): "
read autocrlf
if [ ! "$autocrlf" = "" ] ; then
    git config core.autocrlf $autocrlf
fi

