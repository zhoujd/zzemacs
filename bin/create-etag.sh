#!/bin/bash
#set -x

echo "Clean TAGS"
rm -f TAGS

echo "Build TAGS"
SCAN_LIST=(
    $PWD
    $@
)

find ${SCAN_LIST[*]} \
     \( -not -path '*/.git/*' \) \
     \( -type f \) \
     \( -iname "*.[chly]"    \
     -o -iname "*.[ch]xx"    \
     -o -iname "*.[ch]pp"    \
     -o -iname "*.cc"        \
     -o -iname "*.hh"        \
     \) \
     -print | etags -

ls -lh TAGS
