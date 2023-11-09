#!/bin/bash
#set -x

echo "Clean cscope files"
rm -f cscope*

echo "Generate cscope.files"
SCAN_LIST=(
    $PWD
    $@
)

find ${SCAN_LIST[*]} \
     \( -not -path '*/.git/*' \) \
     \( -type f -a -not -type l \) \
     \( -iname "*.[chly]"    \
     -o -iname "*.[ch]xx"    \
     -o -iname "*.[ch]pp"    \
     -o -iname "*.cc"        \
     -o -iname "*.hh"        \
     \) \
     -print | grep -v " " > cscope.files

echo "Build cscope files"
cscope -b -R -q -k -i cscope.files -f cscope.out
ls -lh cscope.*
