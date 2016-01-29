#!/usr/bin/env python

### parameter desc
##$base $local $other $output

import os
import sys
import platform

merge_tool = ["p4merge", "meld", "bcompare"]
merge_select = merge_tool[0]

sysstr = platform.system()
if sysstr =="Windows":
    merge_select = merge_tool[2]
elif sysstr == "Linux":
    merge_select = merge_tool[0]
else:
    print "Other System tasks"

os.system(merge_select + " " + " ".join(sys.argv[1:]))
