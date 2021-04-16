#!/bin/sh

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

## mergetool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## https://lukas.zapletalovi.com/2012/09/three-way-git-merging-with-meld.html
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/

main() {
    if [ "$OS" = "Windows_NT" ] ; then
        MERGE_TOOL_0="bcompare $*"
        MERGE_TOOL_1="meld $2 $1 $3 -o $4 --diff $1 $2 --diff $1 $3 --auto-merge"
        MERGE_TOOL_2="p4merge $*"

        MERGE_SELECT=$MERGE_TOOL_0
    else
        MERGE_TOOL_0="bcompare $*"
        MERGE_TOOL_1="meld $2 $1 $3 -o $4 --diff $1 $2 --diff $1 $3 --auto-merge"
        MERGE_TOOL_2="p4merge $*"

        MERGE_SELECT=$MERGE_TOOL_2
    fi

    $MERGE_SELECT
}

## run merge tools
main $1 $2 $3 $4
