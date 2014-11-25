#!/usr/bin/env python

import sys, os

patchdir = sys.argv[1]
f = os.popen("ls -1 %s | sort -n" % (patchdir))

for fname in f:
    cmd = "patch -p1 < %s/%s" % (patchdir, fname.strip())
    print cmd
    os.system(cmd)
    
f.close()
