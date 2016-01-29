#!/bin/sh

echo git diff setup start ...

if [ "$OS" = "Windows_NT" ] ; then
    GIT_SETUP_HOME=`pwd -W`
else
    GIT_SETUP_HOME=`pwd`
fi

## clear ~/.gitconfig
echo "remove ~/.gitconfig and setting git configure ..."
rm -f ~/.gitconfig

## setup git configure
git config --global user.name   "zhoujd"
git config --global user.email  "zjd-405@163.com"
git config --global color.ui    "true"

## alias
git config --global alias.st    "status"
git config --global alias.ci    "commit"
git config --global alias.br    "branch"
git config --global alias.co    "checkout"
git config --global alias.df    "difftool"
git config --global alias.dc    "difftool --cached"
git config --global alias.lg    "log -p"
git config --global alias.lol   "log --graph --decorate --pretty=oneline --abbrev-commit"
git config --global alias.lola  "log --graph --decorate --pretty=oneline --abbrev-commit --all"
git config --global alias.ls    "ls-files"
git config --global alias.ign   "ls-files -o -i --exclude-standard"

## set http proxy
if [ ! $http_proxy = "" ]; then
    git config --global http.proxy $http_proxy
fi

### fatal: index-pack failed for win7
#git config --global pack.windowMemory  10m
#git config --global pack.packSizeLimit 20m

### win7 git server (gitblit)
### http://code.google.com/p/gitblit/downloads/detail?name=gitblit-1.0.0.zip
### error: RPC failed; result=18, HTTP code = 0
git config --global http.postBuffer 524288000

### git diff is called by git with 7 parameters:
### path old-file old-hex old-mode new-file new-hex new-mode

## git default diff using external
#chmod +x $GIT_SETUP_HOME/git-diff-default.sh
#git config --global diff.external $GIT_SETUP_HOME/git-diff-default.sh

## git difftool setting
chmod +x $GIT_SETUP_HOME/git-diff-wrapper.sh
git config --global diff.tool extdiff
git config --global difftool.extdiff.cmd "$SHELL $GIT_SETUP_HOME/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
git config --global difftool.prompt false

## setup merge setting
chmod +x $GIT_SETUP_HOME/git-merge-wrapper.sh
git config --global merge.tool extmerge
git config --global mergetool.extmerge.cmd "$SHELL $GIT_SETUP_HOME/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config --global mergetool.extmerge.trustExitCode true
git config --global mergetool.keepBackup false


if [ "$OS" = "Windows_NT" ] ; then
   cp ~/.gitconfig $USERPROFILE
fi


echo git diff setup end ...
