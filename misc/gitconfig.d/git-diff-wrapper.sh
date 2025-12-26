#!/bin/bash

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
    DIFF_TOOLS=(
        "bcompare $*"
        "p4merge $*"
        "meld $*"
    )
    DIFF_SELECT=${DIFF_SELECT:-meld}
    for t in "${DIFF_TOOLS[@]}"; do
        c=(${t})
        if command -v ${c[0]} >/dev/null 2>&1; then
            DIFF_SELECT="${t}"
            break
        fi
    done
    $DIFF_SELECT
}

## run diff tools
main $1 $2
