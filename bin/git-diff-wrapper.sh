#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

## use emacs as diff tool
EMACS_FLAG="n"

## difftool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

if [ "$OS" = "Windows_NT" ] ; then
    DIFF_TOOL="C:/BCompare3/BCompare.exe"
else
    #DIFF_TOOL="bcompare" ##"meld"
    DIFF_TOOL="/home/media/meld/bin/meld"
fi

## run diff tools
case "$EMACS_FLAG" in
    "Y" | "y" )
        emacs --eval "(ediff-files \"$1\" \"$2\")"
        ;;
    * )
        $DIFF_TOOL $*
        ;;
esac

exit 0


