#!/bin/sh

GIT_SETUP_HOME=`pwd`

echo git diff setup end ...

# setup packages
sudo apt-get install -y python-nautilus python-configobj python-gtk2 python-glade2 python-svn python-dbus meld
sudo apt-get install -y python-meld3
sudo apt-get install -y git-core

# setup git configure
git config --global user.name  "zhoujd"          # 姓名全拼 
git config --global user.email "zjd-405@163.com" # 公司邮箱 
git config --global merge.tool "meld" 
git config --global color.ui   true              # 使用 git 默认的配色方案，推荐 git config --
git config --list                                # 查看配置信息 

# setup diff setting
chmod +x $GIT_SETUP_HOME/git-diff-wrapper.sh 
git config --global diff.external $GIT_SETUP_HOME/git-diff-wrapper.sh

echo git diff setup end ...
