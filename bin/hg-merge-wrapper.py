#!/usr/bin/env python

### parameter desc
##$base $local $other $output

import os
import sys
import platform

emacs_flag="y"

def merge_extern(a, b, c, d):
    merge_select = []
    sysstr = platform.system()
    if sysstr == "Windows":
        merge_tool = [
            ["p4merge", a, b, c, d],
            ["bcompare", a, b, c, d],
            ["meld", a, b, d],
        ]

        merge_select = merge_tool[1]
    elif sysstr == "Linux":
        merge_tool = [
            ["p4merge", a, b, c, d],
            ["bcompare", a, b, c, d],
            ["meld", a, b, d],
        ]

        merge_select = merge_tool[2]
    else:
        return merge_emacs(a, b, c, d)

    os.system(" ".join(merge_select))

def merge_emacs(a, b, c, d):
    strfilepath = os.path.realpath(__file__)
    sysstr = platform.system()
    if sysstr == "Windows":
        a = a.replace("\\", "/")
        b = b.replace("\\", "/")
        c = c.replace("\\", "/")
        d = d.replace("\\", "/")
        strfilepath = strfilepath.replace("\\", "/")

    zzemacs_path = "%s/../" % (os.path.dirname(strfilepath),)
    elisp_string="(progn \
                    (load-file \\\"%s/elisp/ediff-sample.el\\\") \
                    (ediff-merge-files \\\"%s\\\" \\\"%s\\\" \\\"%s\\\" \\\"%s\\\") \
                  )" % (zzemacs_path, a, b, c, d)

    cmd = "emacs -q --no-site-file --eval \"%s\"" % elisp_string
    os.system(cmd)

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print "Using example: %s <base> <local> <other> <output>" % sys.argv[0]
        sys.exit(1)

    if emacs_flag == "y" or emacs_flag == "Y":
        merge_emacs(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
    else:
        merge_extern(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
