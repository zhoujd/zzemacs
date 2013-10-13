#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

## use emacs as diff tool
EMACS_FLAG="n"

## difftool selects
if [ "$OS" = "Windows_NT" ] ; then
    DIFF_TOOL="C:/BCompare3/BCompare.exe"
else
    DIFF_TOOL="bcompare" ##"meld"
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


