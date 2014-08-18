#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

## difftool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

if [ "$OS" = "Windows_NT" ] ; then
    DIFF_TOOL_0="sh emacs-diff.sh $*"
    DIFF_TOOL_1="$ZZNIX_HOME/home/zhoujd/zztools/bcompare/bcompare $*"
    DIFF_TOOL_2="$ZZNIX_HOME/home/zhoujd/zztools/perforce/p4merge $*"

    DIFF_SELECT=$DIFF_TOOL_1
else
    DIFF_TOOL_0="sh emacs-diff.sh $*"
    DIFF_TOOL_1="$HOME/zztools/bcompare/bin/bcompare $*"
    DIFF_TOOL_2="$HOME/zztools/meld/bin/meld $*"
    DIFF_TOOL_3="$HOME/zztools/p4v/bin/p4merge $*"

    DIFF_SELECT=$DIFF_TOOL_2
fi

## run diff tools
$DIFF_SELECT
