#!/bin/sh

#DIFF_TOOL="D:/develop/BCompare3/BCompare.exe"
DIFF_TOOL="meld"

# diff is called by git with 7 parameters:
# path old-file old-hex old-mode new-file new-hex new-mode
$DIFF_TOOL "$2" "$5" | cat
