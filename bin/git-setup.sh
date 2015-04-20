#!/bin/sh

### http://gitbook.liuhui998.com/index.html

echo git global setup start ...

if [ "$OS" = "Windows_NT" ] ; then
    GIT_SETUP_HOME=$(cd $(dirname $0) && pwd -W)
    ZZ_ETC_ROOT=$(cd $GIT_SETUP_HOME/../etc && pwd -W)
else
    GIT_SETUP_HOME=$(cd $(dirname $0) && pwd)
    ZZ_ETC_ROOT=$(cd $GIT_SETUP_HOME/../etc && pwd)
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

## set git proxy
git config --global core.gitproxy  $GIT_SETUP_HOME/git-proxy-wrapper.sh
git config --global core.editor    "emacs -Q"

## setup git configure
git config --global user.name   "zhoujd"
git config --global user.email  "zjd-405@163.com"

## output color
git config --global color.ui    "true"

## not check file mode on windows
if [ "$OS" = "Windows_NT" ] ; then
    ## cr && lf
    git config --global core.autocrlf false
    git config --global core.safecrlf true
    git config --global core.filemode false
fi

## alias
git config --global alias.st    "status"
git config --global alias.ci    "commit"
git config --global alias.br    "branch"
git config --global alias.co    "checkout"
git config --global alias.fp    "format-patch"
git config --global alias.df    "difftool"
git config --global alias.dc    "difftool --cached"
git config --global alias.de    "ediff"
git config --global alias.ds    "diff --stat"
git config --global alias.nlog  "log -n"
git config --global alias.llog  "log -n 10"
git config --global alias.lg    "log -p"
git config --global alias.lol   "log --graph --decorate --pretty=oneline --abbrev-commit"
git config --global alias.lola  "log --graph --decorate --pretty=oneline --abbrev-commit --all"
git config --global alias.ls    "ls-files"
git config --global alias.ign   "ls-files -o -i --exclude-standard"
git config --global alias.glog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
git config --global alias.flog  "show --pretty=format: --name-only"

## set http proxy
git config --global http.proxy $http_proxy
git config --global https.proxy $https_proxy
git config --global http.sslcainfo $ZZ_ETC_ROOT/curl-ca-bundle.crt
git config --global credential.helper 'cache --timeout=300'

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
git config --global diff.tool extdiff
git config --global difftool.extdiff.cmd "$GIT_SETUP_HOME/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
git config --global difftool.prompt false

## setup merge setting
git config --global merge.tool extmerge
git config --global mergetool.extmerge.cmd "$GIT_SETUP_HOME/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config --global mergetool.extmerge.trustExitCode true
git config --global mergetool.keepBackup false

## update gitconfig for cmd using
if [ "$OS" = "Windows_NT" ] ; then
    if [ ! $(cd "$HOME" ; pwd -W) = $(cd $USERPROFILE ; pwd -W) ] ; then
        cp -f ~/.gitconfig $USERPROFILE
    fi
fi


echo git global setup end ...
