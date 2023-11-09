#!/bin/bash
#set -x

## Find Option
SCAN_LIST=(
    $PWD
    $@
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
    -a -not -type l
)

echo "Clean cscope files"
rm -f cscope*

echo "Generate scan files"
find ${SCAN_LIST[@]} \
     \( ${EXCLUDE_LIST[@]} \) \
     \( ${TYPE_LIST[@]} \) \
     \( ${FILTER_LIST[@]} \) \
     -print | grep -v " " > cscope.files

echo "Build cscope files"
cscope -b -R -q -k -i cscope.files -f cscope.out
ls -lh cscope.*
