#!/usr/bin/env bash

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

## use emacs as diff tool
EMACS_FLAG="n"
EMACS="runemacs"  ##"emacs"

## difftool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

if [ "$OS" = "Windows_NT" ] ; then
    DIFF_TOOL="C:/BCompare3/BCompare.exe"
else
    #DIFF_TOOL="$HOME/zztools/bcompare/bin/bcompare"
    #DIFF_TOOL="$HOME/zztools/meld/bin/meld"
    DIFF_TOOL="$HOME/zztools/p4v/bin/p4merge"
fi

## run diff tools
case "$EMACS_FLAG" in
    "Y" | "y" )
        $EMACS --eval "(progn (setq ediff-temp-file \"$1\") (ediff-files \"$1\" \"$2\"))"
        ;;
    * )
        $DIFF_TOOL $*
        ;;
esac

exit 0
