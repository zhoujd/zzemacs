#!/usr/bin/env python

### parameter desc

import os
import sys

diff_tool = ["p4merge", "meld", "bcompare"]
os.system(diff_tool[0] + " " + " ".join(sys.argv[1:]))
