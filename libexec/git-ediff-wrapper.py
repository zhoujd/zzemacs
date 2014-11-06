#!/usr/bin/env python

### parameter desc
#$child $parent

import os
import sys
import platform

def diff_emacs(a, b):
    zzemacs_path=""
    sysstr = platform.system()
    if sysstr == "Windows":
        a = a.replace("\\", "/")
        b = b.replace("\\", "/")
        zzemacs_path = os.environ.get('ZZNIX_HOME') + "/home/zhoujd/zzemacs"
    else:
        zzemacs_path = os.environ.get('HOME') + "/zzemacs"

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
