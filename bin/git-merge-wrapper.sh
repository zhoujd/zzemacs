#!/bin/sh

# difftool selects
if [ "$OS" = "Windows_NT" ] ; then
    MERGE_TOOL="C:/BCompare3/BCompare.exe"
else
    MERGE_TOOL="bcompare" ##"meld"
fi

# diff is called by git with 7 parameters:
# path old-file old-hex old-mode new-file new-hex new-mode
$MERGE_TOOL $*

