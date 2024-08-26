#!/bin/sh

echo setup git remote start ...

REMOTE_DIR=$HOME/zach
REMOTE_CFG=$REMOTE_DIR/.gitconfig-zach

mkdir -p $REMOTE_DIR

## basic
git config --file=$REMOTE_CFG user.name   "Zachary Zhou"
git config --file=$REMOTE_CFG user.email  "zchrzhou@gmail.com"
git config --file=$REMOTE_CFG color.ui    "true"

## proxy
git config --file=$REMOTE_CFG http.proxy $http_proxy
git config --file=$REMOTE_CFG http.sslverify false
git config --file=$REMOTE_CFG credential.helper "cache --timeout=3600"

## push
git config --file=$REMOTE_CFG push.default simple

## alias

git config --file=$REMOTE_CFG alias.st    "status"
git config --file=$REMOTE_CFG alias.ci    "commit"
git config --file=$REMOTE_CFG alias.cae   "commit --amend"
git config --file=$REMOTE_CFG alias.ca    "commit --amend --no-edit"
git config --file=$REMOTE_CFG alias.br    "branch"
git config --file=$REMOTE_CFG alias.co    "checkout"
git config --file=$REMOTE_CFG alias.fp    "format-patch"
git config --file=$REMOTE_CFG alias.df    "diff"
git config --file=$REMOTE_CFG alias.dfc   "diff --cached"
git config --file=$REMOTE_CFG alias.dt    "difftool"
git config --file=$REMOTE_CFG alias.dtc   "difftool --cached"
git config --file=$REMOTE_CFG alias.de    "ediff"
git config --file=$REMOTE_CFG alias.dex   "ediffx"
git config --file=$REMOTE_CFG alias.ds    "diff --stat"
git config --file=$REMOTE_CFG alias.mt    "mergetool"
git config --file=$REMOTE_CFG alias.me    "mergetool --tool=emacs"
git config --file=$REMOTE_CFG alias.cp    "cherry-pick"
git config --file=$REMOTE_CFG alias.cpn   "cherry-pick -n"
git config --file=$REMOTE_CFG alias.rb    "rebase"
git config --file=$REMOTE_CFG alias.rs    "reset"
git config --file=$REMOTE_CFG alias.rsh   "reset --hard"
git config --file=$REMOTE_CFG alias.ps    "push"
git config --file=$REMOTE_CFG alias.pl    "pull"
git config --file=$REMOTE_CFG alias.plr   "pull --rebase"
git config --file=$REMOTE_CFG alias.wc    "whatchanged"
git config --file=$REMOTE_CFG alias.ls    "ls-files"
git config --file=$REMOTE_CFG alias.ign   "ls-files -o -i --exclude-standard"
git config --file=$REMOTE_CFG alias.fname "show --pretty=format: --name-only"
git config --file=$REMOTE_CFG alias.dname "diff --pretty=format: --name-only"

## log
git config --file=$REMOTE_CFG alias.glog  "log --graph --pretty=format:'%Cred%h%Creset%C(yellow)%d%Creset %s %C(bold blue)<%an>%Creset %Cgreen(%cr)%Creset' --abbrev-commit"
git config --file=$REMOTE_CFG alias.hlog  'log --oneline'
git config --file=$REMOTE_CFG alias.lg    '!git glog -10'
git config --file=$REMOTE_CFG alias.hg    '!git hlog -10'

## daemon
git config --file=$REMOTE_CFG alias.srv   '!git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose'
git config --file=$REMOTE_CFG alias.hub   '!git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose'

## list aliases
git config --file=$REMOTE_CFG alias.la    "!git config -l | grep alias | cut -c 7-"


## includeif
git config --global --unset-all includeif.gitdir:$REMOTE_DIR/.path
git config --global --add includeif.gitdir:$REMOTE_DIR/.path $REMOTE_CFG


echo setup git remote end ...
