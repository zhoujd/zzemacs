#!/bin/bash
#set -x

## Find Option
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
    -a -not -type l
)

dep() {
    sudo apt install cscope
}

clean() {
    echo "Clean cscope files"
    rm -f cscope*
}

build() {
    SCAN_LIST=(
        ${SCAN_DEF_LIST[@]}
    )

    for dir in $@; do
        if [ -d $dir ]; then
            SCAN_LIST+=($dir)
        fi
    done

    echo "Generate scan files"
    printf '%s\n' "${SCAN_LIST[@]}"
    find ${SCAN_LIST[@]} \
         \( ${EXCLUDE_LIST[@]} \) \
         \( ${TYPE_LIST[@]} \) \
         \( ${FILTER_LIST[@]} \) \
         -print | grep -v " " > cscope.files

    echo "Build cscope files"
    cscope -b -R -q -k -i cscope.files -f cscope.out
    ls -lh cscope.*
}

usage() {
    APP=$(basename $0)
    cat <<EOF
Usage:
$ $APP {build|-b|clean|-c|dep}
$ $APP -b <dir1> <dir2> .. <dirN> ## Use relative path
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
