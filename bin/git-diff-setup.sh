#!/bin/sh

GIT_SETUP_HOME=`pwd`

echo git diff setup start ...

# setup packages
echo -n "Do you need install packages? (y/N): "
read answer
case "$answer" in
    "Y" | "y" )
        sudo apt-get install -y python-nautilus python-configobj python-gtk2 python-glade2 python-svn python-dbus meld
        sudo apt-get install -y python-meld3
        sudo apt-get install -y git-core
        sudo apt-get install -y gitk
esac

# setup git configure
git config --global user.name  "zhoujd"
git config --global user.email "zjd-405@163.com"
#git config --global merge.tool "kdiff3" 
#git config --global merge.tool "meld"
git config --global color.ui   true

# setup diff setting
chmod +x $GIT_SETUP_HOME/git-diff-wrapper.sh 
git config --global diff.external $GIT_SETUP_HOME/git-diff-wrapper.sh

echo ===========git config start ===============
git config --list
echo ===========git config end =================


echo git diff setup end ...
