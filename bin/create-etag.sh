#!/bin/sh

echo "Clean TAGS"
rm -f TAGS

echo "Build TAGS"
find $@ \
     /usr/include \
     $PWD \
     \( -not -path '*/.git/*' \) \
     \( -type f \) \
     \( -name "*.[chCH]"    \
     -o -name "*.cc"        \
     -o -name "*.[ch]xx"    \
     -o -name "*.[ch]pp"    \
     -o -name "*.CC"        \
     -o -name "*.HH"        \
     -o -name "*.[ch]++"    \
     \) \
     -print | etags -
ls -lh TAGS
