#!/bin/sh

FIND=find

$FIND $@ -type f \( -name "*.[chCH]"  \
                 -o -name "*.cc"      \
                 -o -name "*.[ch]xx"  \
                 -o -name "*.[ch]pp"  \
                 -o -name "*.CC"      \
                 -o -name "*.HH"      \
                 -o -name "*.[ch]++"  \
                 \) -print | etags -

ls -l TAGS
