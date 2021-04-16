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


main() {
    if [ "$OS" = "Windows_NT" ] ; then
        DIFF_TOOL_0="bcompare $*"
        DIFF_TOOL_1="meld $*"
        DIFF_TOOL_2="p4merge $*"
        DIFF_TOOL_3="vim -d $*"

        DIFF_SELECT=$DIFF_TOOL_0
    else
        DIFF_TOOL_0="bcompare $*"
        DIFF_TOOL_1="meld $*"
        DIFF_TOOL_2="p4merge $*"
        DIFF_TOOL_3="vim -d $*"

        DIFF_SELECT=$DIFF_TOOL_2
    fi

    $DIFF_SELECT
}

## run diff tools
main $1 $2
