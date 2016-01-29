#!/usr/bin/env python

### parameter desc

import os
import sys
import platform

diff_tool = ["p4merge", "meld", "bcompare"]
diff_select = diff_tool[0]

sysstr = platform.system()
if sysstr =="Windows":
    diff_select = diff_tool[2]
elif sysstr == "Linux":
    diff_select = diff_tool[1]
else:
    print "Other System tasks"

os.system(diff_select + " " + " ".join(sys.argv[1:]))
