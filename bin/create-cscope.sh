#!/bin/sh

FIND=find

$FIND $@ -type f \( -name "*.[chCH]"  \
                 -o -name "*.cc"      \
                 -o -name "*.[ch]xx"  \
                 -o -name "*.[ch]pp"  \
                 -o -name "*.CC"      \
                 -o -name "*.HH"      \
                 -o -name "*.[ch]++"  \
                 \) -print > cscope.files;

cscope -b -R -q -i cscope.files

ls -l cscope.*
