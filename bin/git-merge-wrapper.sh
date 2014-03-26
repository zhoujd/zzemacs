#!/usr/bin/env bash

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

## use emacs as diff tool
EMACS_FLAG="y"
EMACS="runemacs"

## mergetool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

if [ "$OS" = "Windows_NT" ] ; then
    MERGE_TOOL="C:/BCompare3/BCompare.exe"
else
    #MERGE_TOOL="$HOME/zztools/bcompare/bin/bcompare"
    #MERGE_TOOL="$HOME/zztools/meld/bin/meld"
    MERGE_TOOL="$HOME/zztools/p4v/bin/p4merge"
fi

## run merge tools
case "$EMACS_FLAG" in
    "Y" | "y" )
        $EMACS --eval "(progn
                        (setq emerge-temp-local-file  \"$2\"
                              emerge-temp-base-file   \"$3\"
                              emerge-temp-remote-file \"$1\")
                        (ediff-merge-files-with-ancestor \"$2\" \"$3\" \"$1\" nil \"$4\"))"
        ;;
    * )
        $MERGE_TOOL $*
        ;;
esac

exit 0
