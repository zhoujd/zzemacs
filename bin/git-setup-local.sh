#!/bin/sh

### http://gitbook.liuhui998.com/index.html

echo git local setup start ...

if [ "$OS" = "Windows_NT" ] ; then
    GIT_SETUP_HOME=$(cd $(dirname $0) && pwd -W)
else
    GIT_SETUP_HOME=$(cd $(dirname $0) && pwd)
fi

## set git proxy
git config core.gitproxy  $GIT_SETUP_HOME/git-proxy-wrapper.sh
git config core.editor    "emacs -Q"

## setup git configure
git config user.name   "zhoujd"
git config user.email  "zjd-405@163.com"
git config color.ui    "true"

## alias
git config alias.st    "status"
git config alias.ci    "commit"
git config alias.br    "branch"
git config alias.co    "checkout"
git config alias.fp    "format-patch"
git config alias.df    "difftool"
git config alias.dc    "difftool --cached"
git config alias.ds    "diff --stat"
git config alias.de    "ediff"
git config alias.nlog  "log -n"
git config alias.llog  "log -n 10"
git config alias.lg    "log -p"
git config alias.lol   "log --graph --decorate --pretty=oneline --abbrev-commit"
git config alias.lola  "log --graph --decorate --pretty=oneline --abbrev-commit --all"
git config alias.ls    "ls-files"
git config alias.ign   "ls-files -o -i --exclude-standard"
git config alias.glog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
git config alias.flog  "show --pretty=format: --name-only"

## git difftool setting
git config diff.tool extdiff
git config difftool.extdiff.cmd "$GIT_SETUP_HOME/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
git config difftool.prompt false

## setup merge setting
git config merge.tool extmerge
git config mergetool.extmerge.cmd "$GIT_SETUP_HOME/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config mergetool.extmerge.trustExitCode true
git config mergetool.keepBackup false


echo git local setup end ...
