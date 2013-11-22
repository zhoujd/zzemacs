#!/bin/sh

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

## use emacs as diff tool
EMACS_FLAG="n"

## mergetool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html

if [ "$OS" = "Windows_NT" ] ; then
    MERGE_TOOL="C:/BCompare3/BCompare.exe"
else
    MERGE_TOOL="bcompare" ##"meld"
fi

## run merge tools
case "$EMACS_FLAG" in
    "Y" | "y" )
        emacs --eval "(ediff-merge-files-with-ancestor \"$1\" \"$2\" \"$3\" nil \"$4\")"
        ;;
    * )
        $MERGE_TOOL $*
        ;;
esac

exit 0

