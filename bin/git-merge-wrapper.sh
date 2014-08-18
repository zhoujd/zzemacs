#!/bin/sh

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

## mergetool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

if [ "$OS" = "Windows_NT" ] ; then
    MERGE_TOOL_0="sh emacs-merge.sh $*"
    MERGE_TOOL_1="$ZZNIX_HOME/home/zhoujd/zztools/bcompare/bcompare $*"
    MERGE_TOOL_2="$ZZNIX_HOME/home/zhoujd/zztools/perforce/p4merge $*"

    MERGE_SELECT=$MERGE_TOOL_1
else
    MERGE_TOOL_0="sh emacs-merge.sh $*"
    MERGE_TOOL_1="$HOME/zztools/bcompare/bin/bcompare $*"
    MERGE_TOOL_2="$HOME/zztools/meld/bin/meld $2 $4 $3"
    MERGE_TOOL_3="$HOME/zztools/p4v/bin/p4merge $*"

    if [ "Linux" = "`uname -s`" ]; then
        MERGE_SELECT=$MERGE_TOOL_3
    else
        MERGE_SELECT=$MERGE_TOOL_0
    fi    
fi

## run merge tools
$MERGE_SELECT
