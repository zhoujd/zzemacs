#!/bin/sh

### http://gitbook.liuhui998.com/index.html

echo git local setup start ...

if [ "$OS" = "Windows_NT" ] ; then
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd -W)
else
    SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
fi

## set git proxy
git config core.gitproxy  $SCRIPT_ROOT/git-proxy-wrapper.sh
git config core.editor    $SCRIPT_ROOT/git-editor.sh

## setup git configure
git config user.name   "Zachary Zhou"
git config user.email  "zachary.zhou@hotmail.com"
git config color.ui    "true"

## alias
git config alias.st    "status"
git config alias.ci    "commit"
git config alias.cae   "commit --amend"
git config alias.ca    "commit --amend --no-edit"
git config alias.br    "branch"
git config alias.co    "checkout"
git config alias.fp    "format-patch"
git config alias.df    "diff"
git config alias.dfc   "diff --cached"
git config alias.dt    "difftool"
git config alias.dtc   "difftool --cached"
git config alias.de    "ediff"
git config alias.dex   "ediffx"
git config alias.ds    "diff --stat"
git config alias.mt    "mergetool"
git config alias.cp    "cherry-pick"
git config alias.cpn   "cherry-pick -n"
git config alias.rb    "rebase"
git config alias.rs    "reset"
git config alias.rsh   "reset --hard"
git config alias.ps    "push"
git config alias.pl    "pull"
git config alias.pr    "pull --rebase"
git config alias.wc    "whatchanged"
git config alias.ls    "ls-files"
git config alias.ign   "ls-files -o -i --exclude-standard"
git config alias.cat   "cat-file -p"
git config alias.flog  "show --pretty=format: --name-only"
git config alias.last  "log -1 HEAD"
git config alias.glog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
git config alias.hlog  "log --graph --pretty=format:'%Cred%h%Creset %C(cyan)%an%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative -10"
git config alias.rb    "rebase"

## git difftool setting
git config diff.tool extdiff
git config difftool.extdiff.cmd "$SCRIPT_ROOT/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
git config difftool.prompt false

## setup merge setting
git config merge.tool extmerge
git config mergetool.extmerge.cmd "$SCRIPT_ROOT/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git config mergetool.extmerge.trustExitCode true
git config mergetool.keepBackup false


echo git local setup end ...
