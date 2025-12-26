#!/bin/bash

### parameter desc
## $1 => $BASE
## $2 => $LOCAL
## $3 => $REMOTE
## $4 => $MERGED

## mergetool selects
## http://www.scootersoftware.com/support.php?c=kb_vcs.php
## http://www.perforce.com/perforce/products/merge.html
## http://meldmerge.org/
## https://lukas.zapletalovi.com/2012/09/three-way-git-merging-with-meld.html

main() {
    MERGE_TOOLS=(
        "bcompare $*"
        "p4merge $*"
        "meld $2 $1 $3 -o $4 --diff $1 $2 --diff $1 $3 --auto-merge"
    )
    MERGE_SELECT=${MERGE_SELECT:-meld}
    for t in "${MERGE_TOOLS[@]}"; do
        c=(${t})
        if command -v ${c[0]} >/dev/null 2>&1; then
            MERGE_SELECT="${t}"
            break
        fi
    done
    $MERGE_SELECT
}

## run merge tools
main $1 $2 $3 $4
