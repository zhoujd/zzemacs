#!/bin/sh

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

## use emacs as diff tool
EMACS_FLAG="n"
EMACS="runemacs"

## mergetool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

if [ "$OS" = "Windows_NT" ] ; then
    ZZTOOLS_ROOT="c:/zznix/home/zhoujd/zztools"

    MERGE_TOOL="$ZZTOOLS_ROOT/bcompare/bcompare"
    ARGS="$*"

    #MERGE_TOOL="$ZZTOOLS_ROOT/perforce/p4merge"
    #ARGS="$*"
else
    ZZTOOLS_ROOT="$HOME/zztools"

    #MERGE_TOOL="$ZZTOOLS_ROOT/bcompare/bin/bcompare"
    #ARGS="$*"

    #MERGE_TOOL="$ZZTOOLS_ROOT/meld/bin/meld"
    #ARGS="$2 $4 $3"

    MERGE_TOOL="$ZZTOOLS_ROOT/p4v/bin/p4merge"
    ARGS="$*"
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
        $MERGE_TOOL $ARGS
        ;;
esac

exit 0
