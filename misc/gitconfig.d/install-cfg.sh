#!/bin/bash

SCRIPT_ROOT=$(cd $(dirname $0) && pwd)
CFG_DEF=~/.gitconfig
OPT=(
    --global
)

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
        echo "$(basename $0) {--file|-f|--help|-h|--local|-l|*}"
        exit 0
        ;;
    --local | -l )
        OPT=(
        )
        ;;
    * )
        init
        ;;
esac

echo "git global setup start"

## run git config
git_cfg() {
    git config ${OPT[*]} "$@"
}

## core
git_cfg core.gitproxy  $SCRIPT_ROOT/git-proxy-wrapper.sh
git_cfg core.editor    $SCRIPT_ROOT/git-editor.sh

## user
git_cfg user.name   "Zachary Zhou"
git_cfg user.email  "zchrzhou@gmail.com"

## color
git_cfg color.ui    "true"

## alias
git_cfg alias.st    "status"
git_cfg alias.ci    "commit"
git_cfg alias.cae   "commit --amend"
git_cfg alias.ca    "commit --amend --reset-author --no-edit"
git_cfg alias.br    "branch"
git_cfg alias.co    "checkout"
git_cfg alias.fp    "format-patch"
git_cfg alias.df    "diff"
git_cfg alias.dfc   "diff --cached"
git_cfg alias.dt    "difftool"
git_cfg alias.dtc   "difftool --cached"
git_cfg alias.de    "ediff"
git_cfg alias.dex   "ediffx"
git_cfg alias.ds    "diff --stat"
git_cfg alias.mt    "mergetool"
git_cfg alias.me    "mergetool --tool=emacs"
git_cfg alias.mn    "merge --no-ff"
git_cfg alias.ms    "merge --squash"
git_cfg alias.cp    "cherry-pick"
git_cfg alias.cpa   "cherry-pick --abort"
git_cfg alias.cpc   "cherry-pick --continue"
git_cfg alias.cpn   "cherry-pick -n"
git_cfg alias.rb    "rebase"
git_cfg alias.rba   "rebase --abort"
git_cfg alias.rbc   "rebase --continue"
git_cfg alias.rbi   "rebase -i"
git_cfg alias.rs    "reset"
git_cfg alias.rsh   "reset --hard"
git_cfg alias.ps    "push"
git_cfg alias.pl    "pull"
git_cfg alias.plr   "pull --rebase"
git_cfg alias.wc    "whatchanged"
git_cfg alias.addp  "add -p"
git_cfg alias.who   "blame -wMC"
git_cfg alias.sta   "stash apply"
git_cfg alias.stc   "stash clear"
git_cfg alias.std   "stash drop"
git_cfg alias.stl   "stash list --pretty=format:'%Cblue%gd%Cred: %C(yellow)%s'"
git_cfg alias.stp   "stash pop"
git_cfg alias.sts   "stash show --text"
git_cfg alias.ls    "ls-files"
git_cfg alias.ign   "ls-files -o -i --exclude-standard"
git_cfg alias.fname "show --pretty=format: --name-only"
git_cfg alias.dname "diff --pretty=format: --name-only"

## log
git_cfg alias.glog  "log --graph --pretty=format:'%Cred%h%Creset%C(yellow)%d%Creset %s %C(bold blue)<%an>%Creset %Cgreen(%cr)%Creset' --abbrev-commit"
git_cfg alias.hlog  'log --oneline'
git_cfg alias.lg    '!git glog -10'
git_cfg alias.hg    '!git hlog -10'

## daemon
git_cfg alias.srv   '!git daemon --base-path=. --export-all --reuseaddr --informative-errors --verbose'
git_cfg alias.hub   '!git daemon --base-path=. --export-all --enable=receive-pack --reuseaddr --informative-errors --verbose'

## list aliases
git_cfg alias.la    "!git config -l | grep alias | cut -c 7-"

## set http/https proxy
git_cfg http.proxy $http_proxy

## cp /c/Git/mingw64/ssl/certs/ca-bundle.crt $SCRIPT_ROOT/cert/ca-bundle.crt
## git_cfg http.sslcainfo $SCRIPT_ROOT/cert/ca-bundle.crt
## export GIT_SSL_NO_VERIFY=1
## git_cfg http.sslverify false

## http://stackoverflow.com/questions/11693074/git-credential-cache-is-not-a-git-command
## sudo apt install ca-certificates
git_cfg credential.helper "cache --timeout=3600"

### git diff is called by git with 7 parameters:
### path old-file old-hex old-mode new-file new-hex new-mode

## git default diff using external
#git_cfg diff.external $SCRIPT_ROOT/git-diff-default.sh

## git difftool setting
git_cfg diff.tool extdiff
git_cfg difftool.extdiff.cmd "$SCRIPT_ROOT/git-diff-wrapper.sh \"\$LOCAL\" \"\$REMOTE\""
git_cfg difftool.prompt false

## setup merge setting
git_cfg merge.tool extmerge

git_cfg mergetool.extmerge.cmd "$SCRIPT_ROOT/git-merge-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git_cfg mergetool.extmerge.trustExitCode false

git_cfg mergetool.emacs.cmd "$SCRIPT_ROOT/git-emergex-wrapper.sh \"\$BASE\" \"\$LOCAL\" \"\$REMOTE\" \"\$MERGED\""
git_cfg mergetool.emacs.trustExitCode false

git_cfg mergetool.keepBackup false

git_cfg push.default simple

## setup URLs
URL=~/.gitconfig-url
touch $URL
git_cfg --add include.path $URL


echo "git global setup end"
