#!/bin/bash
#set -x

# Find Option
SCAN_DEF_LIST=(
)

EXCLUDE_LIST=(
    -not -path "/*.git/*"
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
    echo "$(basename $0) {build|-b|clean|-c|dep}"
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
