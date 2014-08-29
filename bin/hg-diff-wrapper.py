#!/usr/bin/env python

### parameter desc
#$child $parent

import os
import sys
import platform

emacs_flag="y"

def diff_extern(a, b):
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
        diff_select = ["sh emacs-diff.sh", a, b]

    os.system(" ".join(diff_select))

def diff_emacs(a, b):
    a = a.replace("\\", "/")
    b = b.replace("\\", "/")

    elisp_string="\
(progn \
 (load-file \\\"c:/zznix/home/zhoujd/zzemacs/elisp/ediff-sample.el\\\") \
 (ediff-sample-diff \\\"%s\\\" \\\"%s\\\") \
 )" % (a, b)

    cmd = "emacs -q --no-site-file --eval \"%s\"" % elisp_string
    print cmd
    os.system(cmd)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Using example: %s <diff-A> <diff-B>" % sys.argv[0]
        sys.exit(1)

    if emacs_flag == "y" or emacs_flag == "Y":
       diff_emacs(sys.argv[1], sys.argv[2])
    else:
       diff_extern(sys.argv[1], sys.argv[2])
