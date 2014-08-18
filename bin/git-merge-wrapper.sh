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
    ZZTOOLS_ROOT="$ZZNIX_HOME/home/zhoujd/zztools"

    MERGE_TOOL="$ZZTOOLS_ROOT/bcompare/bcompare"
    ARGS="$*"

    #MERGE_TOOL="$ZZTOOLS_ROOT/perforce/p4merge"
    #ARGS="$*"

    #MERGE_TOOL="sh emacs-merge.sh"
    #ARGS="$*"
else
    ZZTOOLS_ROOT="$HOME/zztools"

    #MERGE_TOOL="$ZZTOOLS_ROOT/bcompare/bin/bcompare"
    #ARGS="$*"

    #MERGE_TOOL="$ZZTOOLS_ROOT/meld/bin/meld"
    #ARGS="$2 $4 $3"

    MERGE_TOOL="$ZZTOOLS_ROOT/p4v/bin/p4merge"
    ARGS="$*"

    #MERGE_TOOL="sh emacs-merge.sh"
    #ARGS="$*"
fi

## run merge tools
$MERGE_TOOL $ARGS

