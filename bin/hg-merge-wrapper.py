#!/usr/bin/env python

### parameter desc
##$base $local $other $output

import os
import sys

merge_tool = "p4merge" ##bcompare
os.system(merge_tool + " " + " ".join(sys.argv[1:]))

