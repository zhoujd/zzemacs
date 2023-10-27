#!/bin/sh

echo "Clean cscope files"
rm -f cscope*

echo "Generate cscope.files"
find $@ \
     \( -not -path '*/.git/*' \) \
     \( -type f -a -not -type l \) \
     \( -name "*.[chCH]"    \
     -o -name "*.cc"        \
     -o -name "*.[ch]xx"    \
     -o -name "*.[ch]pp"    \
     -o -name "*.CC"        \
     -o -name "*.HH"        \
     -o -name "*.[ch]++"    \
     \) \
     -print | grep -v " " > cscope.files

echo "Build cscope files"
cscope -b -R -q -k -i cscope.files -f cscope.out
ls -lh cscope.*
