#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
CFG_DEF=~/.gitconfig

init() {
    CFG=${1:-$CFG_DEF}
    echo "remove $CFG and setting git configure ..."
    rm -f $CFG
    cat <<EOF > $CFG
#
# This is the config file, and
# a '#' or ';' character indicates
# a comment
#

EOF
}

case $1 in
    --file | -f )
        FILE=${2:-"gitconfig"}
        OPT=(
            -f $FILE
        )
        init $FILE
        ;;
    --help | -h )
        echo "$(basename $0) {--file|-f|--help|-h|--global|-g|*}"
        exit 0
        ;;
    --local | -l )
        OPT=(
        )
        ;;
    * )
        OPT=(
            --global
        )
        init
        ;;
esac

echo "git global setup start"

CMD="git config ${OPT[*]}"

## core
$CMD core.gitproxy  $SCRIPT_ROOT/git-proxy-wrapper.sh
$CMD core.editor    $SCRIPT_ROOT/git-editor.sh

## user
$CMD user.name   "Zachary Zhou"
$CMD user.email  "zchrzhou@gmail.com"

## color
$CMD color.ui    "true"

## alias
$CMD alias.st    "status"
$CMD alias.ci    "commit"
$CMD alias.cae   "commit --amend"
$CMD alias.ca    "commit --amend --reset-author --no-edit"
$CMD alias.br    "branch"
$CMD alias.co    "checkout"
$CMD alias.fp    "format-patch"
$CMD alias.df    "diff"
$CMD alias.dfc   "diff --cached"
$CMD alias.dt    "difftool"
$CMD alias.dtc   "difftool --cached"
$CMD alias.de    "ediff"
$CMD alias.dex   "ediffx"
$CMD alias.ds    "diff --stat"
$CMD alias.mt    "mergetool"
$CMD alias.me    "mergetool --tool=emacs"
$CMD alias.mn    "merge --no-ff"
$CMD alias.ms    "merge --squash"
$CMD alias.cp    "cherry-pick"
$CMD alias.cpa   "cherry-pick --abort"
$CMD alias.cpc   "cherry-pick --continue"
$CMD alias.cpn   "cherry-pick -n"
$CMD alias.rb    "rebase"
$CMD alias.rba   "rebase --abort"
$CMD alias.rbc   "rebase --continue"
$CMD alias.rbi   "rebase -i"
$CMD alias.rs    "reset"
$CMD alias.rsh   "reset --hard"
$CMD alias.ps    "push"
$CMD alias.pl    "pull"
$CMD alias.plr   "pull --rebase"
$CMD alias.wc    "whatchanged"
$CMD alias.addp  "add -p"
$CMD alias.who   "blame -wMC"
$CMD alias.sta   "stash apply"
$CMD alias.stc   "stash clear"
$CMD alias.std   "stash drop"
$CMD alias.stl   "stash list --pretty=format:'%Cblue%gd%Cred: %C(yellow)%s'"
$CMD alias.stp   "stash pop"
$CMD alias.sts   "stash show --text"
$CMD alias.ls    "ls-files"
$CMD alias.ign   "ls-files -o -i --exclude-standard"
$CMD alias.fname "show --pretty=format: --name-only"
$CMD alias.dname "diff --pretty=format: --name-only"

## log
$CMD alias.glog  "log --graph --pretty=format:'%Cred%h%Creset%C(yellow)%d%Creset %s %C(bold blue)<%an>%Creset %Cgreen(%cr)%Creset' --abbrev-commit"
$CMD alias.hlog  'log --oneline'
$CMD alias.lg    '!git glog -10'
$CMD alias.hg    '!git hlog -10'

## daemon
$CMD alias.srv   '!git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose'
$CMD alias.hub   '!git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose'

## list aliases
$CMD alias.la    "!git config -l | grep alias | cut -c 7-"

## set http/https proxy
$CMD http.proxy $http_proxy

## cp /c/Git/mingw64/ssl/certs/ca-bundle.crt $SCRIPT_ROOT/cert/ca-bundle.crt
## $CMD http.sslcainfo $SCRIPT_ROOT/cert/ca-bundle.crt
## export GIT_SSL_NO_VERIFY=1
## $CMD http.sslverify false

## http://stackoverflow.com/questions/11693074/git-credential-cache-is-not-a-git-command
## sudo apt install ca-certificates
$CMD credential.helper "cache --timeout=3600"

### git diff is called by git with 7 parameters:
### path old-file old-hex old-mode new-file new-hex new-mode

## git default diff using external
#$CMD diff.external $SCRIPT_ROOT/git-diff-default.sh

## git difftool setting
$CMD diff.tool extdiff
$CMD difftool.extdiff.cmd "$SCRIPT_ROOT/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
$CMD difftool.prompt false

## setup merge setting
$CMD merge.tool extmerge

$CMD mergetool.extmerge.cmd "$SCRIPT_ROOT/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
$CMD mergetool.extmerge.trustExitCode false

$CMD mergetool.emacs.cmd "$SCRIPT_ROOT/git-emergex-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
$CMD mergetool.emacs.trustExitCode false

$CMD mergetool.keepBackup false

$CMD push.default simple

## setup URLs
## requires git v1.7.10+
GITCONFIG_URL=~/.gitconfig-url
touch $GITCONFIG_URL
$CMD --add include.path $GITCONFIG_URL


echo "git global setup end"
