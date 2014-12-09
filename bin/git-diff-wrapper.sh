#!/bin/sh

### parameter desc
## $1 => $LOCAL
## $2 => $REMOTE

## difftool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/
## http://ftp.gnome.org/pub/GNOME/sources/meld/3.12/
## https://git.gnome.org/browse/meld/

## use emacs as diff tool
EMACS_FLAG="y"

diff_extern()
{
    if [ "$OS" = "Windows_NT" ] ; then
        DIFF_TOOL_0="bcompare $*"
        DIFF_TOOL_1="meld $*"
        DIFF_TOOL_2="p4merge $*"

        DIFF_SELECT=$DIFF_TOOL_1
    else
        DIFF_TOOL_0="bcompare $*"
        DIFF_TOOL_1="meld $*"
        DIFF_TOOL_2="p4merge $*"

        DIFF_SELECT=$DIFF_TOOL_1
    fi

    $DIFF_SELECT
}

diff_emacs()
{
    if [ "$OS" = "Windows_NT" ] ; then
        ELISP_PATH="$(cd $(dirname $0)/../elisp && pwd -W)"
    else
        ELISP_PATH="$(cd $(dirname $0)/../elisp && pwd)"
    fi

    emacs --quick \
          --eval "(load-file \"$ELISP_PATH/ediff-sample.el\")" \
          --eval "(ediff-sample-diff \"$1\" \"$2\")" \
          --eval "(message \"emacs diff finished.\")"
}

main()
{
    case "$EMACS_FLAG" in
        "Y" | "y" )
            diff_emacs $*
            ;;
        * )
            diff_extern $*
            ;;
    esac
}

## run diff tools
main $1 $2
