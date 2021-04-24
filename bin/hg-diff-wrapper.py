#!/usr/bin/env python3

# parameter desc
# $child $parent

import os
import sys
import platform

emacs_flag = "y"

def diff_extern(a, b):
    diff_select = []
    sysstr = platform.system()
    if sysstr == "Windows":
        diff_tool = [
            ["meld", a, b],
            ["p4merge", a, b],
            ["bcompare", a, b],
        ]

        diff_select = diff_tool[1]
    elif sysstr == "Linux":
        diff_tool = [
            ["p4merge", a, b],
            ["bcompare", a, b],
            ["meld", a, b],
        ]

        diff_select = diff_tool[2]
    else:
        return diff_emacs(a, b)

    os.system(" ".join(diff_select))


def diff_emacs(a, b):
    strfilepath = os.path.realpath(__file__)
    sysstr = platform.system()
    if sysstr == "Windows":
        a = a.replace("\\", "/")
        b = b.replace("\\", "/")
        strfilepath = strfilepath.replace("\\", "/")

    zzemacs_path = "%s/../" % (os.path.dirname(strfilepath),)
    elisp_string = "(progn \
                    (load-file \\\"%s/elisp/ediff-sample.el\\\") \
                    (ediff-sample-diff \\\"%s\\\" \\\"%s\\\") \
                  )" % (zzemacs_path, a, b)

    cmd = "emacs --quick --eval \"%s\"" % elisp_string
    os.system(cmd)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Using example: %s <diff-A> <diff-B>" % sys.argv[0])
        sys.exit(1)

    if emacs_flag == "y" or emacs_flag == "Y":
       diff_emacs(sys.argv[1], sys.argv[2])
    else:
       diff_extern(sys.argv[1], sys.argv[2])
