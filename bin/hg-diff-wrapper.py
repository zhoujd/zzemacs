#!/usr/bin/env python

### parameter desc
#$child $parent

import os
import sys
import platform

def hg_diff(a, b):
    diff_select = []
    sysstr = platform.system()
    if sysstr == "Windows":
        zztools_home = os.environ.get('ZZNIX_HOME') + "/home/zhoujd/zztools"
        diff_tool = [
            [zztools_home + "/perforce/p4merge", a, b], 
            [zztools_home + "/bcompare/bcompare", a, b],
        ]

        diff_select = diff_tool[1]
    elif sysstr == "Linux":
        zztools_home = os.environ.get('HOME') + "/zztools"
        diff_tool = [
            [zztools_home + "/p4v/bin/p4merge", a, b], 
            [zztools_home + "/bcompare/bin/bcompare", a, b],
            [zztools_home + "/meld/bin/meld", a, b],
        ]

        diff_select = diff_tool[2]
    else:
        merge_tool = ["echo unsupport platform"]

    return " ".join(diff_select)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Using example: %s <diff-A> <diff-B>" % sys.argv[0]
        sys.exit(1)

    os.system(hg_diff(sys.argv[1], sys.argv[2]))
