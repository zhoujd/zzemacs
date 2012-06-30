#!/bin/sh

if [ "$OS" = "Windows_NT" ] ; then
    DIFF_TOOL="D:/develop/BCompare3/BCompare.exe"
else
    DIFF_TOOL="meld"
fi

# diff is called by git with 7 parameters:
# path old-file old-hex old-mode new-file new-hex new-mode
$DIFF_TOOL "$2" "$5" | cat
