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
## sudo yum install openssl-devel libcurl-devel expat-devel perl-devel
## wget https://github.com/git/git/archive/v2.16.6.zip
## unzip v2.16.6.zip
## cd git-2.16.6
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
git config --global user.email  "zchrzhou@gmail.com"

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
git config --global alias.df    "diff"
git config --global alias.dfc   "diff --cached"
git config --global alias.dt    "difftool"
git config --global alias.dtc   "difftool --cached"
git config --global alias.de    "ediff"
git config --global alias.dex   "ediffx"
git config --global alias.ds    "diff --stat"
git config --global alias.mt    "mergetool"
git config --global alias.mn    "merge --no-ff"
git config --global alias.cp    "cherry-pick"
git config --global alias.cpn   "cherry-pick -n"
git config --global alias.rb    "rebase"
git config --global alias.rs    "reset"
git config --global alias.rsh   "reset --hard"
git config --global alias.ps    "push"
git config --global alias.pl    "pull"
git config --global alias.plr   "pull --rebase"
git config --global alias.wc    "whatchanged"
git config --global alias.ls    "ls-files"
git config --global alias.ign   "ls-files -o -i --exclude-standard"
git config --global alias.cat   "cat-file -p"
git config --global alias.flog  "show --pretty=format: --name-only"
git config --global alias.last  "log -1 HEAD"
git config --global alias.glog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
git config --global alias.hlog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative -10"
git config --global alias.addp  "add -p"
git config --global alias.hx    "blame"

## git daemon
git config --global alias.serve '!git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose'
git config --global alias.hub   '!git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose'

## set http/https proxy
git config --global http.proxy $http_proxy

## cp /c/Git/mingw64/ssl/certs/ca-bundle.crt $MISC_ROOT/ca-bundle.crt
## git config --global http.sslcainfo $MISC_ROOT/ca-bundle.crt
## export GIT_SSL_NO_VERIFY=1
git config --global http.sslverify false

## http://stackoverflow.com/questions/11693074/git-credential-cache-is-not-a-git-command
if [ "$OS" = "Windows_NT" ] ; then
    git config --global credential.helper wincred
else
    ## sudo apt install ca-certificates
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
