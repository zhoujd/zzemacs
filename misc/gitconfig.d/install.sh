#!/bin/bash

echo "git global setup start"

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)

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

## alias
git config --global alias.st    "status"
git config --global alias.ci    "commit"
git config --global alias.cae   "commit --amend"
git config --global alias.ca    "commit --amend --reset-author --no-edit"
git config --global alias.br    "branch"
git config --global alias.co    "checkout"
git config --global alias.fp    "format-patch"
git config --global alias.ftp   "fetch --prune"
git config --global alias.df    "diff"
git config --global alias.dfc   "diff --cached"
git config --global alias.dt    "difftool"
git config --global alias.dtc   "difftool --cached"
git config --global alias.de    "ediff"
git config --global alias.dex   "ediffx"
git config --global alias.ds    "diff --stat"
git config --global alias.mt    "mergetool"
git config --global alias.me    "mergetool --tool=emacs"
git config --global alias.mn    "merge --no-ff"
git config --global alias.ms    "merge --squash"
git config --global alias.cp    "cherry-pick"
git config --global alias.cpa   "cherry-pick --abort"
git config --global alias.cpc   "cherry-pick --continue"
git config --global alias.cpn   "cherry-pick -n"
git config --global alias.rb    "rebase"
git config --global alias.rba   "rebase --abort"
git config --global alias.rbc   "rebase --continue"
git config --global alias.rbi   "rebase -i"
git config --global alias.rs    "reset"
git config --global alias.rsh   "reset --hard"
git config --global alias.ps    "push"
git config --global alias.pl    "pull"
git config --global alias.plr   "pull --rebase"
git config --global alias.wc    "whatchanged"
git config --global alias.addp  "add -p"
git config --global alias.who   "blame -wMC"
git config --global alias.sta   "stash apply"
git config --global alias.stc   "stash clear"
git config --global alias.std   "stash drop"
git config --global alias.stl   "stash list --pretty=format:'%Cblue%gd%Cred: %C(yellow)%s'"
git config --global alias.stp   "stash pop"
git config --global alias.sts   "stash show --text"
git config --global alias.ls    "ls-files"
git config --global alias.ign   "ls-files -o -i --exclude-standard"
git config --global alias.dn    "diff --pretty=format: --name-only"
git config --global alias.sn    "show --pretty=format: --name-only"

## log
PRETTY="format:'%Cred%h%Creset%C(yellow)%d%Creset %s %C(bold blue)<%an>%Creset %Cgreen(%cr)%Creset'"
git config --global alias.glog  "log --graph --pretty=$PRETTY --abbrev-commit"
git config --global alias.lg    "log --graph --pretty=$PRETTY --abbrev-commit -10"  
git config --global alias.hlog  'log --oneline'
git config --global alias.hg    'log --oneline -10'

## list aliases
git config --global alias.la    "!git config -l | grep alias | cut -c 7-"

## set http/https proxy
git config --global http.proxy $http_proxy

## cp /c/Git/mingw64/ssl/certs/ca-bundle.crt $SCRIPT_ROOT/cert/ca-bundle.crt
## git config --global http.sslcainfo $SCRIPT_ROOT/cert/ca-bundle.crt
## export GIT_SSL_NO_VERIFY=1
## git config --global http.sslverify false

## http://stackoverflow.com/questions/11693074/git-credential-cache-is-not-a-git-command
## sudo apt install ca-certificates
git config --global credential.helper "cache --timeout=3600"

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
git config --global mergetool.extmerge.trustExitCode false
git config --global mergetool.emacs.cmd "$SCRIPT_ROOT/git-emergex-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config --global mergetool.emacs.trustExitCode false
git config --global mergetool.keepBackup false

## git will display merge conflicts with the contents of the merge base as well
git config --global merge.conflictStyle zdiff3

## git push setting
git config --global push.default simple

## setup URL
GITCONFIG_URL=~/.gitconfig-url
touch $GITCONFIG_URL
git config --global --add include.path $GITCONFIG_URL

## setup WS
GITCONFIG_WS=~/.gitconfig-work
touch $GITCONFIG_WS
git config --global includeif.gitdir:~/work/.path $GITCONFIG_WS


echo "git global setup end"
