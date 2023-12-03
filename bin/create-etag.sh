#!/bin/bash
#set -x

# apt install emacs-bin-common

# Find Option
SCAN_LIST=(
    .
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
)

echo "Clean TAGS"
rm -f TAGS

echo "Build TAGS"
find ${SCAN_LIST[@]} \
     \( ${EXCLUDE_LIST[@]} \) \
     \( ${TYPE_LIST[@]} \) \
     \( ${FILTER_LIST[@]} \) \
     -print | etags -

ls -lh TAGS
