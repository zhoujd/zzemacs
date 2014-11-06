#!/bin/sh

### http://gitbook.liuhui998.com/index.html

echo git diff setup start ...

if [ "$OS" = "Windows_NT" ] ; then
    GIT_SETUP_HOME=`pwd -W`
    SHELL=$(cd $ZZNIX_HOME && pwd -W)/bin/sh
else
    GIT_SETUP_HOME=$(dirname $0)
fi

## clear ~/.gitconfig
echo "remove ~/.gitconfig and setting git configure ..."
rm -f ~/.gitconfig

## add title
cat > ~/.gitconfig <<EOF
#
# This is the config file, and
# a '#' or ';' character indicates
# a comment
#

EOF

## setup git configure
git config --global user.name   "zhoujd"
git config --global user.email  "zjd-405@163.com"
git config --global color.ui    "true"

## cr && lf
git config --global core.autocrlf false
git config --global core.safecrlf true
git config --global core.filemode false

## alias
git config --global alias.st    "status"
git config --global alias.ci    "commit"
git config --global alias.br    "branch"
git config --global alias.co    "checkout"
git config --global alias.df    "difftool"
git config --global alias.dc    "difftool --cached"
git config --global alias.de    "ediff"
git config --global alias.nlog  "log -n"
git config --global alias.llog  "log -n 10"
git config --global alias.lg    "log -p"
git config --global alias.lol   "log --graph --decorate --pretty=oneline --abbrev-commit"
git config --global alias.lola  "log --graph --decorate --pretty=oneline --abbrev-commit --all"
git config --global alias.ls    "ls-files"
git config --global alias.ign   "ls-files -o -i --exclude-standard"
git config --global alias.glog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(blue)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

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
    if [ ! $(cd "$HOME" ; pwd -W) = $(cd $USERPROFILE ; pwd -W) ] ; then
        cp -f ~/.gitconfig $USERPROFILE
    fi
fi


echo git diff setup end ...
