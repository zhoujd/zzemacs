#!/bin/bash
#set -x

# Find Option
SCAN_DEF_LIST=(
)

EXCLUDE_LIST=(
    -not -path "/*.git/*"
    -not -path "/*.ccls/*"
)

FILTER_LIST=(
    -iname "*.[chly]"   
    -o -iname "*.[ch]xx"
    -o -iname "*.[ch]pp"
    -o -iname "*.cc"    
    -o -iname "*.hh"    
)

TYPE_LIST=(
    -type f
)

dep() {
    sudo apt install emacs-bin-common
}

clean() {
    echo "Clean TAGS"
    rm -f TAGS
}

build() {
    SCAN_LIST=(
        ${SCAN_DEF_LIST[@]}
        $@
    )
    echo "Build TAGS"
    find ${SCAN_LIST[@]} \
         \( ${EXCLUDE_LIST[@]} \) \
         \( ${TYPE_LIST[@]} \) \
         \( ${FILTER_LIST[@]} \) \
         -print | etags -

    ls -lh TAGS
}

usage() {
    cat <<EOF 
Usage:
$ $(basename $0) {build|-b|clean|-c|dep}
$ $(basename $0) -b <dir1> <dir2> .. <dirN> ## Use relative path
EOF
}

case $1 in
    dep )
        dep
        ;;
    -b | build )
        shift
        build $@
        ;;
    -c | clean )
        clean
        ;;
    * )
        usage
        ;;
esac
