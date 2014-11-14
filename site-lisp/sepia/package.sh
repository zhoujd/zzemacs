#!/bin/sh

# cat MANIFEST.in > MANIFEST
# ls doc/*.html >> MANIFEST
# perl Makefile.PL && make && make test && make dist
perl Makefile.PL && make && make dist
perl -Mblib -MSepia -i -pe 's/^(\@set VERSION ).*/$1$Sepia::VERSION/' sepia.texi
makeinfo sepia.texi -o Sepia.info
makeinfo --no-split --no-headers --html sepia.texi -o Sepia.html
