#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
OPT=${1:-"--global"}  ## or --local

echo "git global setup start"

Init() {        
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
}

case $OPT in
    --global )
        Init
        ;;
    * )
        echo "Setup for $OPT"
        ;;
esac

## set git proxy
git config $OPT core.gitproxy  $SCRIPT_ROOT/git-proxy-wrapper.sh
git config $OPT core.editor    $SCRIPT_ROOT/git-editor.sh

## setup git configure
git config $OPT user.name   "Zachary Zhou"
git config $OPT user.email  "zchrzhou@gmail.com"

## output color
git config $OPT color.ui    "true"

## alias
git config $OPT alias.st    "status"
git config $OPT alias.ci    "commit"
git config $OPT alias.cae   "commit --amend"
git config $OPT alias.ca    "commit --amend --reset-author --no-edit"
git config $OPT alias.br    "branch"
git config $OPT alias.co    "checkout"
git config $OPT alias.fp    "format-patch"
git config $OPT alias.df    "diff"
git config $OPT alias.dfc   "diff --cached"
git config $OPT alias.dt    "difftool"
git config $OPT alias.dtc   "difftool --cached"
git config $OPT alias.de    "ediff"
git config $OPT alias.dex   "ediffx"
git config $OPT alias.ds    "diff --stat"
git config $OPT alias.mt    "mergetool"
git config $OPT alias.me    "mergetool --tool=emacs"
git config $OPT alias.mn    "merge --no-ff"
git config $OPT alias.ms    "merge --squash"
git config $OPT alias.cp    "cherry-pick"
git config $OPT alias.cpa   "cherry-pick --abort"
git config $OPT alias.cpc   "cherry-pick --continue"
git config $OPT alias.cpn   "cherry-pick -n"
git config $OPT alias.rb    "rebase"
git config $OPT alias.rba   "rebase --abort"
git config $OPT alias.rbc   "rebase --continue"
git config $OPT alias.rbi   "rebase -i"
git config $OPT alias.rs    "reset"
git config $OPT alias.rsh   "reset --hard"
git config $OPT alias.ps    "push"
git config $OPT alias.pl    "pull"
git config $OPT alias.plr   "pull --rebase"
git config $OPT alias.wc    "whatchanged"
git config $OPT alias.addp  "add -p"
git config $OPT alias.who   "blame -wMC"
git config $OPT alias.sta   "stash apply"
git config $OPT alias.stc   "stash clear"
git config $OPT alias.std   "stash drop"
git config $OPT alias.stl   "stash list --pretty=format:'%Cblue%gd%Cred: %C(yellow)%s'"
git config $OPT alias.stp   "stash pop"
git config $OPT alias.sts   "stash show --text"
git config $OPT alias.ls    "ls-files"
git config $OPT alias.ign   "ls-files -o -i --exclude-standard"
git config $OPT alias.fname "show --pretty=format: --name-only"
git config $OPT alias.dname "diff --pretty=format: --name-only"

## log
git config $OPT alias.glog  "log --graph --pretty=format:'%Cred%h%Creset%C(yellow)%d%Creset %s %C(bold blue)<%an>%Creset %Cgreen(%cr)%Creset' --abbrev-commit"
git config $OPT alias.hlog  'log --oneline'
git config $OPT alias.lg    '!git glog -10'
git config $OPT alias.hg    '!git hlog -10'

## daemon
git config $OPT alias.srv   '!git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose'
git config $OPT alias.hub   '!git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose'

## list aliases
git config $OPT alias.la    "!git config -l | grep alias | cut -c 7-"

## set http/https proxy
git config $OPT http.proxy $http_proxy

## cp /c/Git/mingw64/ssl/certs/ca-bundle.crt $SCRIPT_ROOT/cert/ca-bundle.crt
## git config $OPT http.sslcainfo $SCRIPT_ROOT/cert/ca-bundle.crt
## export GIT_SSL_NO_VERIFY=1
## git config $OPT http.sslverify false

## http://stackoverflow.com/questions/11693074/git-credential-cache-is-not-a-git-command
## sudo apt install ca-certificates
git config $OPT credential.helper "cache --timeout=3600"

### git diff is called by git with 7 parameters:
### path old-file old-hex old-mode new-file new-hex new-mode

## git default diff using external
#git config $OPT diff.external $SCRIPT_ROOT/git-diff-default.sh

## git difftool setting
git config $OPT diff.tool extdiff
git config $OPT difftool.extdiff.cmd "$SCRIPT_ROOT/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
git config $OPT difftool.prompt false

## setup merge setting
git config $OPT merge.tool extmerge

git config $OPT mergetool.extmerge.cmd "$SCRIPT_ROOT/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config $OPT mergetool.extmerge.trustExitCode false

git config $OPT mergetool.emacs.cmd "$SCRIPT_ROOT/git-emergex-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config $OPT mergetool.emacs.trustExitCode false

git config $OPT mergetool.keepBackup false

git config $OPT push.default simple

## setup URLs
## requires git v1.7.10+
GITCONFIG_URL=~/.gitconfig-url
touch $GITCONFIG_URL
git config $OPT --add include.path $GITCONFIG_URL


echo "git global setup end"
