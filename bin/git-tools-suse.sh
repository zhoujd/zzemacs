#!/bin/sh

GIT_SETUP_HOME=`pwd`

echo git diff tools for suse setup start ...

## setup packages
if [ "$OS" != "Windows_NT" ] ; then
    echo -n "Do you need install packages? (y/N): "
    read answer
    case "$answer" in
        "Y" | "y" )
            sudo zypper install git gitk
    esac
fi

echo git diff tools for suse setup end ...
