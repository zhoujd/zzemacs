#!/usr/bin/env python

### parameter desc

import os
import sys

diff_tool = "p4merge" ##bcompare
os.system(diff_tool + " " + " ".join(sys.argv[1:]))
