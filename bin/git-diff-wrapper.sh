#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE


## difftool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

if [ "$OS" = "Windows_NT" ] ; then
    ZZTOOLS_ROOT="$ZZNIX_HOME/home/zhoujd/zztools"

    DIFF_TOOL="$ZZTOOLS_ROOT/bcompare/bcompare"
    #DIFF_TOOL="$ZZTOOLS_ROOT/perforce/p4merge"
    #DIFF_TOOL="sh emacs-diff.sh"
else
    ZZTOOLS_ROOT="$HOME/zztools"

    #DIFF_TOOL="$ZZTOOLS_ROOT/bcompare/bin/bcompare"
    #DIFF_TOOL="$ZZTOOLS_ROOT/meld/bin/meld"
    DIFF_TOOL="$ZZTOOLS_ROOT/p4v/bin/p4merge"
    #DIFF_TOOL="sh emacs-diff.sh"
fi

## run diff tools

$DIFF_TOOL $*

