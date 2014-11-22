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

## use emacs as merge tool
EMACS_FLAG="y"

merge_extern()
{
    if [ "$OS" = "Windows_NT" ] ; then
        MERGE_TOOL_0="bcompare $*"
        MERGE_TOOL_1="p4merge $*"

        MERGE_SELECT=$MERGE_TOOL_0
    else
        MERGE_TOOL_0="bcompare $*"
        MERGE_TOOL_1="meld $2 $4 $3"
        MERGE_TOOL_2="p4merge $*"

        MERGE_SELECT=$MERGE_TOOL_2
    fi

    $MERGE_SELECT
}

merge_emacs()
{
    if [ "$OS" = "Windows_NT" ] ; then
        ELISP_PATH="$(cd $(dirname $0)/../elisp && pwd -W)"
    else
        ELISP_PATH="$(cd $(dirname $0)/../elisp && pwd)"
    fi

    emacs --no-site-file -q \
          --eval "(load-file \"$ELISP_PATH/ediff-sample.el\")" \
          --eval "(ediff-merge-files \"$2\" \"$3\" \"$1\" \"$4\")" \
          --eval "(message \"emacs merge finished.\")"
}

main()
{
    case "$EMACS_FLAG" in
        "Y" | "y" )
            merge_emacs $*
            ;;
        * )
            merge_extern $*
            ;;
    esac
}

## run merge tools
main $1 $2 $3 $4
