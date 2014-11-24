#!/usr/bin/env python

### parameter desc
#$child $parent

import os
import sys
import platform

def diff_emacs(a, b):
    strfilepath = os.path.realpath(__file__)
    sysstr = platform.system()
    if sysstr == "Windows":
        a = a.replace("\\", "/")
        b = b.replace("\\", "/")
        strfilepath = strfilepath.replace("\\", "/")

    zzemacs_path = "%s/../" % (os.path.dirname(strfilepath),)
    elisp_string="(progn \
                    (load-file \\\"%s/elisp/ediff-sample.el\\\") \
                    (ediff-sample-diff \\\"%s\\\" \\\"%s\\\") \
                  )" % (zzemacs_path, a, b)

    cmd = "emacs -q --no-site-file --eval \"%s\"" % elisp_string
    os.system(cmd)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Using example: %s <diff-A> <diff-B>" % sys.argv[0]
        sys.exit(1)

    diff_emacs(sys.argv[1], sys.argv[2])
