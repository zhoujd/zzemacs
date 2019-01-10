#!/bin/sh

### http://gitbook.liuhui998.com/index.html
## git submodule add url path/to/name 
## git submodule init
## git submodule foreach git pull
## git submodule update
## git cherry-pick xxx-commit-id
## git pull --squash another
## git update-index --assume-unchanged /path/to/file 
## git update-index --no-assume-unchanged /path/to/file
## find -type d -empty -exec touch {}/.gitignore \;

### Build from source
## sudo apt-get install libssl-dev libcurl4-openssl-dev libexpat1-dev
## wget https://github.com/git/git/releases/tag/v1.8.4.3
## tar xvzf v1.8.4.3.tar.gz
## cd git-v1.8.4.3
## make prefix=/opt/git all
## make prefix=/opt/git install
## echo /opt/git/bin >> /etc/environment # or others

### Get work root
## git rev-parse --show-toplevel

echo git global setup start ...

if [ "$OS" = "Windows_NT" ] ; then
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd -W)
    MISC_ROOT=$(cd $SCRIPT_ROOT/../misc && pwd -W)
else
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
    MISC_ROOT=$(cd $SCRIPT_ROOT/../misc && pwd)
fi

## clear ~/.gitconfig
echo "remove ~/.gitconfig and setting git configure ..."
rm -f ~/.gitconfig

## add title
cat <<EOF > ~/.gitconfig
#
# This is the config file, and
# a '#' or ';' character indicates
# a comment
#

EOF

## set git proxy
git config --global core.gitproxy  $SCRIPT_ROOT/git-proxy-wrapper.sh
git config --global core.editor    $SCRIPT_ROOT/git-editor.sh

## setup git configure
git config --global user.name   "Zachary Zhou"
git config --global user.email  "zachary.zhou@hotmail.com"

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
git config --global alias.cae   "commit --amend"
git config --global alias.ca    "commit --amend --no-edit"
git config --global alias.br    "branch"
git config --global alias.co    "checkout"
git config --global alias.fp    "format-patch"
git config --global alias.df    "difftool"
git config --global alias.dc    "difftool --cached"
git config --global alias.de    "ediff"
git config --global alias.dex   "ediffx"
git config --global alias.ds    "diff --stat"
git config --global alias.mt    "mergetool"
git config --global alias.cp    "cherry-pick"
git config --global alias.cpn   "cherry-pick -n"
git config --global alias.rb    "rebase"
git config --global alias.pr    "pull --rebase"
git config --global alias.ls    "ls-files"
git config --global alias.ign   "ls-files -o -i --exclude-standard"
git config --global alias.cat   "cat-file -p"
git config --global alias.flog  "show --pretty=format: --name-only"
git config --global alias.last  "log -1 HEAD"
git config --global alias.glog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
git config --global alias.hlog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative -10"

## set http/https proxy
git config --global http.proxy $http_proxy
git config --global https.proxy $http_proxy
git config --global http.sslcainfo $MISC_ROOT/ca-bundle.crt

## http://stackoverflow.com/questions/11693074/git-credential-cache-is-not-a-git-command
if [ "$OS" = "Windows_NT" ] ; then
    git config --global credential.helper wincred
else
    git config --global credential.helper "cache --timeout=3600"
fi

### git diff is called by git with 7 parameters:
### path old-file old-hex old-mode new-file new-hex new-mode

## git default diff using external
#git config --global diff.external $SCRIPT_ROOT/git-diff-default.sh

## git difftool setting
git config --global diff.tool extdiff
git config --global difftool.extdiff.cmd "$SCRIPT_ROOT/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
git config --global difftool.prompt false

## setup merge setting
git config --global merge.tool extmerge
git config --global mergetool.extmerge.cmd "$SCRIPT_ROOT/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config --global mergetool.extmerge.trustExitCode true
git config --global mergetool.keepBackup false

git config --global push.default simple

## update gitconfig for cmd using
if [ "$OS" = "Windows_NT" ] ; then
    if [ ! $(cd "$HOME" ; pwd -W) = $(cd $USERPROFILE ; pwd -W) ] ; then
        cp -f ~/.gitconfig $USERPROFILE
    fi
fi

echo git global setup end ...
